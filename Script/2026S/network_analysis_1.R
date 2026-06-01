# =============================================================================
# Wine Trade Network (refactored)
# HS 2204, 2205 / flow = Export
#
# 핵심 변경점
#   1) strength 가중치를 원본 net_wgt_total 로 환원
#        -> net_strength = (총수출 kg) - (총수입 kg) = 순수출 kg  (해석 복원)
#   2) edge_weight_min 은 '시각화 전용'. 중심성은 전체 네트워크에서 계산
#   3) betweenness distance 를 단일 로그로 (이중 로그 압축 제거)
#   4) 2-트랙 구성: 구조 그래프(HS 합산·전체) / 시각화 그래프(HS 분리·필터)
#   5) dead code 제거, turbo->정성형 팔레트, layout stress 통일
# =============================================================================

library(tidyverse)
library(tidygraph)
library(ggraph)
library(graphlayouts)
library(scales)

# ---- 0. 설정 ----------------------------------------------------------------
url_trade   <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/wine_export_2020_2025.csv"
url_country <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"

target_hs       <- c(2204, 2205)
time_filter     <- 2024
noise_floor     <- 0          # 중심성용: 0 또는 잡음 제거용 작은 값(예: 1000 kg)
edge_weight_min <- 10000000        # 시각화 전용 임계치 (kg)

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
  mutate(distance = 1 / log1p(net_wgt_total))   # 단일 로그, 단조 감소, 0/Inf 없음

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
    
    # strength: 원본 kg 가중 -> 합이 곧 총물량
    # out_strength = centrality_degree(mode = "out", weights = log1p(net_wgt_total)),
    # in_strength  = centrality_degree(mode = "in",  weights = log1p(net_wgt_total)),
    out_strength_raw = centrality_degree(mode = "out", weights = net_wgt_total),
    out_strength = log1p(out_strength_raw),
    
    in_strength_raw  = centrality_degree(mode = "in",  weights = net_wgt_total),
    in_strength = log1p(in_strength_raw),
    
    net_strength = out_strength - in_strength,        # = 순수출 kg
    
    eigen_w_raw  = centrality_eigen(directed = TRUE, weights = net_wgt_total),
    eigen_w = log1p(eigen_w_raw),
    
    # PageRank: 거대 단일 흐름 독식 방지가 필요하면 weights=log1p(net_wgt_total) 로 교체
    # pagerank_w   = centrality_pagerank(directed = TRUE, weights = log1p(net_wgt_total)),
    pagerank_w_raw   = centrality_pagerank(directed = TRUE, weights = net_wgt_total),
    pagerank_w  = log1p(pagerank_w_raw),
    
    hub_w_raw  = centrality_hub(weights = net_wgt_total),
    hub_w  = log1p(hub_w_raw),
    
    # betweenness: distance(=1/log1p(w)) 기반, 큰 흐름일수록 '가까움'
    betweenness_w = centrality_betweenness(directed = TRUE, weights = distance),
    
    component = group_components(type = "weak")
  )

# ---- 6. 중심성 테이블 -------------------------------------------------------
centrality_tbl <- g %>% activate(nodes) %>% as_tibble() %>%
  select(name, country, region,
         degree_out, degree_in,
         out_strength, out_strength_raw, 
         in_strength, in_strength_raw, 
         net_strength,
         eigen_w, pagerank_w, betweenness_w, component) %>%
  arrange(desc(out_strength))

print(head(centrality_tbl, 15))

# 참고: net_strength 양수=순수출국 / 음수=순수입국 으로 바로 해석 가능
# centrality_tbl %>% arrange(desc(net_strength)) %>% head(10)   # 순수출 허브
# centrality_tbl %>% arrange(net_strength)       %>% head(10)   # 순수입 허브

# ---- 7. 시각화 그래프: 중심성은 g 에서 가져오고, 엣지는 edges_viz 사용 ------
node_attr <- g %>% activate(nodes) %>% as_tibble()

g_viz <- tbl_graph(nodes = node_attr, edges = edges_viz, directed = TRUE) %>%
  activate(nodes) %>%
  filter(!node_is_isolated())

# ---- 8. 플롯 함수 -----------------------------------------------------------
plot_trade_network <- function(graph,
                               layout_algo = "stress",
                               seed = 2024) {
  set.seed(seed)
  ggraph(graph, layout = layout_algo) +
    geom_edge_fan(
      aes(edge_alpha = std_w,
          edge_width = std_w,
          colour     = as.factor(cmd_code)),
      arrow   = arrow(length = unit(3, "mm"), type = "closed"),
      end_cap = circle(3, "mm"),
      show.legend = TRUE
    ) +
    # 노드 크기: 의미를 명확히 하려면 log1p(out_strength) 권장 (물량 기반)
    # geom_node_point(aes(size = log1p(out_strength), fill = region),
    geom_node_point(aes(size = out_strength_raw, fill = region),
                    shape = 21, colour = "white", stroke = 0.3, alpha = 0.9) +
    geom_node_text(aes(label = country), size = 2.8,
                   repel = TRUE, max.overlaps = 20, family = "sans") +
    scale_size_continuous(range = c(2, 12), guide = "none") +
    scale_edge_width_continuous(range = c(0.2, 2), guide = "none") +
    scale_edge_alpha_continuous(range = c(0.15, 0.8), guide = "none") +
    scale_edge_colour_brewer(palette = "Set1", name = "HS code") +
    scale_fill_brewer(palette = "Set2", name = "Region") +   # 범주형: 정성형 팔레트
    labs(
      title    = "Wine Export Network",
      subtitle = sprintf("Year %d | HS %s | edges > %s kg | total netweight width",
                         time_filter,
                         paste(target_hs, collapse = ", "),
                         format(edge_weight_min, big.mark = ",")),
      caption  = sprintf("Layout: %s | Node size: out-strength", layout_algo)
    ) +
    theme_graph(base_family = "sans") +
    theme(legend.position = "right")
}

# ---- 9. 출력 ----------------------------------------------------------------
plot_trade_network(g_viz, layout_algo = "sphere")
# plot_trade_network(g_viz, layout_algo = "stress")
# plot_trade_network(g_viz, layout_algo = "kk")
