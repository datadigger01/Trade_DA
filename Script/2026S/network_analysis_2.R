# =============================================================================
# Wine Trade Network ??? 2번(process) + 3번(visNetwork 시각화) 결합본
# HS 2204, 2205 / flow = Export
#
#  - 데이터/전처리/중심성: network_analysis_2.R 프로세스를 그대로 따름
#       (전체 데이터, 2-트랙 edges, strength/net_strength/pagerank/betweenness)
#  - 시각화: network_analysis_3.R 의 visNetwork 인터랙티브 블록을 그대로 사용
#  - 연결 키: 노드 id = ISO(2번 방식), label = 국가명  → edges_viz 와 매칭
# =============================================================================

library(tidyverse)
library(tidygraph)        # 중심성 계산 (centrality_*)
library(scales)
library(graphlayouts)     # visIgraphLayout 에서 stress 사용 가능
if (!require("visNetwork")) { install.packages("visNetwork") }
library(visNetwork)

# ---- 0. 설정 (network_analysis_2.R 와 동일) ---------------------------------
url_trade   <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/wine_export_2020_2025.csv"
url_country <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"

target_hs       <- c(2204, 2205)
time_filter     <- 2021
noise_floor     <- 0          # 중심성용: 0 또는 잡음 제거용 작은 값(예: 1000 kg)
edge_weight_min <- 1e7        # 시각화 전용 임계치 (kg)

# ---- 1. 로드 ----------------------------------------------------------------
df_raw       <- read_csv(url_trade,   show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
country_info <- read_csv(url_country, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# ---- 2. 공통 베이스 (시각화 임계치는 여기 적용하지 않음) --------------------
base <- df_raw %>%
  filter(
    cmdCode %in% target_hs,
    flowCode == "X",
    period == time_filter,
    netWgt > 0,
    netWgt > noise_floor,
    reporterISO %in% country_info$iso_3,
    partnerISO  %in% country_info$iso_3
  )

# ---- 3. (A) 구조 엣지: HS 합산 -> 국가쌍당 1엣지 (중심성용) ------------------
edges_struct <- base %>%
  group_by(from = reporterISO, to = partnerISO) %>%
  summarise(net_wgt_total = sum(netWgt, na.rm = TRUE),
            n_obs = n(), .groups = "drop") %>%
  filter(from != to) %>%
  mutate(distance = 1 / log1p(net_wgt_total))

# ---- 3. (B) 시각화 엣지: HS 분리 + 표시 임계치 ------------------------------
edges_viz <- base %>%
  group_by(from = reporterISO, to = partnerISO, cmd_code = cmdCode) %>%
  summarise(net_wgt_total = sum(netWgt, na.rm = TRUE), .groups = "drop") %>%
  filter(from != to, net_wgt_total > edge_weight_min) %>%
  mutate(std_w = rescale(log10(net_wgt_total), to = c(0, 1)))

# ---- 4. 노드 (구조 엣지 기준) -----------------------------------------------
nodes <- tibble(name = unique(c(edges_struct$from, edges_struct$to))) %>%
  left_join(
    country_info %>% select(iso_3, country = name, region, sub_region),
    by = c("name" = "iso_3")
  )

# ---- 5. 구조 그래프 + 중심성 (전체 네트워크 기준) ---------------------------
g <- tbl_graph(nodes = nodes, edges = edges_struct, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(
    degree_out   = centrality_degree(mode = "out"),
    degree_in    = centrality_degree(mode = "in"),
    
    out_strength_raw = centrality_degree(mode = "out", weights = net_wgt_total),
    out_strength = log1p(out_strength_raw),
    in_strength_raw  = centrality_degree(mode = "in",  weights = net_wgt_total),
    in_strength = log1p(in_strength_raw),
    net_strength = out_strength - in_strength,
    eigen_w_raw  = centrality_eigen(directed = TRUE, weights = net_wgt_total),
    eigen_w = log1p(eigen_w_raw),
    pagerank_w_raw   = centrality_pagerank(directed = TRUE, weights = net_wgt_total),
    pagerank_w  = log1p(pagerank_w_raw),
    hub_w_raw  = centrality_hub(weights = net_wgt_total),
    hub_w  = log1p(hub_w_raw),
    betweenness_w = centrality_betweenness(directed = TRUE, weights = distance),
    
    component = group_components(type = "weak")
  )

# ---- 6. 중심성 테이블 -------------------------------------------------------
centrality_tbl <- g %>% activate(nodes) %>% as_tibble() %>%
  select(name, country, region,
         degree_out, degree_in,
         out_strength, in_strength, net_strength,
         eigen_w,pagerank_w, betweenness_w, component) %>%
  arrange(desc(out_strength))

print(head(centrality_tbl, 15))

# 노드 속성 테이블 (visNetwork 노드로 변환할 원본)
node_attr <- g %>% activate(nodes) %>% as_tibble()

# =============================================================================
#  여기부터: network_analysis_3.R 의 visNetwork 시각화 (그대로 살림)
#  - 2번 프로세스 산출물(edges_viz, node_attr)을 vis.js 포맷으로 변환만 함
# =============================================================================

# ---- 7. edges (vis.js 포맷) -------------------------------------------------
#  from/to 는 ISO 코드 (노드 id 와 매칭), tooltip 표시는 국가명으로
edges_vis <- edges_viz %>%
  left_join(country_info %>% select(iso_3, from_desc = name), by = c("from" = "iso_3")) %>%
  left_join(country_info %>% select(iso_3, to_desc   = name), by = c("to"   = "iso_3")) %>%
  transmute(
    from, to,
    # 물량 skew → log 변환 후 width 매핑
    width = rescale(log10(net_wgt_total), to = c(0.5, 8)),
    title = paste0(from_desc, " &#8594; ", to_desc, "<br>",
                   format(round(net_wgt_total), big.mark = ","), " kg<br>HS ", cmd_code),
    color = if_else(cmd_code == 2204, "#377eb8", "#e41a1c")  # HS code 별 색
  )

# ---- 8. nodes (필터된 edge 에 등장하는 국가만) ------------------------------
used_ids <- union(edges_vis$from, edges_vis$to)

nodes_vis <- node_attr %>%
  filter(name %in% used_ids) %>%
  mutate(
    id    = name,
    label = country,
    group = replace_na(region, "Unknown"),
    value = degree_out + 1,                      # 노드 크기 = out-degree (3번 방식 유지)
    title = paste0("<b>", country, "</b><br>region: ", group,
                   "<br>out-degree: ", degree_out,
                   "<br>net-strength: ", format(round(net_strength), big.mark = ","))
  ) %>%
  select(id, label, group, value, title)

# ---- 9. 인터랙티브 시각화 (network_analysis_3.R 와 동일) --------------------
visNetwork(nodes_vis, edges_vis,
           main    = "Wine Export Network (HS 2204 · 2205)",
           submain = sprintf("edges > %s kg · 드래그/줌/hover/이웃 하이라이트",
                             format(edge_weight_min, big.mark = ","))) %>%
  # graphlayouts 가 로드되어 있으면 stress, 아니면 layout_with_fr 로 교체
  visIgraphLayout(layout = "layout_with_fr") %>%
  # visIgraphLayout(layout = "layout_with_stress") %>%
  # visIgraphLayout(layout = "layout_in_circle") %>%
  
  visNodes(shape = "dot",
           shadow = list(enabled = TRUE, size = 8),
           color  = list(border = "white")) %>%
  visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
           smooth = list(enabled = TRUE, type = "curvedCW"),
           color  = list(opacity = 0.45)) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             nodesIdSelection = TRUE,
             selectedBy = "group") %>%
  visLegend(useGroups = TRUE, main = "Region", position = "right") %>%
  visLayout(randomSeed = 2024)

# 저장하려면:
# net <- visNetwork(...) %>% ...
# visSave(net, file = "wine_network.html")