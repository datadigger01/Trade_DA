# =============================================================================
# Wine Trade Network ??? 연도별(2020~2025) 중심성 추이
# + Serrano, Boguna, Vespignani (2009) Disparity Filter 적용 옵션
# HS 2204, 2205 / flow = Export
#
#  - 연도별 전처리·중심성 로직을 compute_centrality_year() 함수로 묶음
#  - Disparity Filter 는 apply_disparity_filter() 로 분리 (backbone 패키지 비의존)
#  - alpha 매개변수로 'full network' vs 'backbone' 전환 가능
#  - 2020~2025 각 연도에 반복 적용 -> long 패널(centrality_panel) 로 저장
#  - 연도별 CSV 저장 + 상위 N 개국 중심성 추이 선그래프
# =============================================================================

library(tidyverse)
library(tidygraph)
library(scales)

# ---- 0. 설정 ----------------------------------------------------------------
url_trade   <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/wine_export_2020_2025.csv"
url_country <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"

target_hs    <- c(2204, 2205)
years        <- 2020:2025      # 분석 대상 연도
noise_floor  <- 0              # 잡음 제거용 최저값 (예: 1000 kg)

# --- Disparity Filter 설정 ---------------------------------------------------
# alpha = 1.0  : 필터 미적용 (원본 네트워크 전체)
# alpha = 0.05 : 표준 통계적 유의수준
# alpha = 0.01 : 더 엄격한 backbone (소수의 핵심 edge 만 보존)
# 작을수록 backbone 이 작아짐. 보통 0.01 ~ 0.5 사이에서 민감도 분석을 권장.
disparity_alpha <- 0.05

focus_metric <- "out_strength" # 상위국 선정/추이 지표
#   선택지: degree_out, degree_in, out_strength,
#           in_strength, net_strength, pagerank_w,
#           eigen_w, hub_w, betweenness_w
top_n        <- 10

# ---- 1. 로드 (전체 연도 데이터를 한 번만 로드) ------------------------------
df_raw       <- read_csv(url_trade,   show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
country_info <- read_csv(url_country, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# ---- 2a. Disparity Filter 헬퍼 ----------------------------------------------
# Serrano M.A., Boguna M., Vespignani A. (2009) PNAS 106(16):6483-6488
# "Extracting the multiscale backbone of complex weighted networks"
#
# 아이디어:
#   각 노드 i 에 대해, edge ij 의 가중치 비율 p_ij = w_ij / s_i 가
#   "k_i 개 edge 에 균등 무작위 분배" null 대비 얼마나 두드러지는지 검정.
#   null 하에서 p_ij 이상이 관측될 확률(p-value): α_ij = (1 - p_ij)^(k_i - 1)
#   사용자 지정 유의수준 α 보다 작으면 그 edge 를 backbone 에 보존.
#
# 방향 그래프:
#   같은 edge 에 대해 from 쪽(out 검정)과 to 쪽(in 검정)을 각각 수행하고,
#   **둘 중 하나라도 유의하면 보존**(OR 규칙, 논문의 기본 권장).
#   덕분에 거대 노드 입장에서 사소해 보여도 작은 노드 입장에서 핵심인 edge 가
#   살아남아 multiscale 특성을 가짐.
#
# 주의: k_i = 1 인 경우 p = 1, (1-1)^0 = 1 이 되어 자기 쪽에서는 통과 못함.
#       하지만 OR 규칙 덕에 반대편에서 통과하면 보존됨. 실제 무역망에서는
#       k = 1 노드 자체가 드물어 거의 문제되지 않음.
# -----------------------------------------------------------------------------
apply_disparity_filter <- function(edges, alpha = 0.05,   weight_col = "net_wgt_total") {
  w <- rlang::sym(weight_col)
  
  out_stats <- edges %>%
    group_by(from) %>%
    summarise(s_out = sum(!!w), k_out = n(), .groups = "drop")
  
  in_stats <- edges %>%
    group_by(to) %>%
    summarise(s_in = sum(!!w), k_in = n(), .groups = "drop")
  
  edges %>%
    left_join(out_stats, by = "from") %>%
    left_join(in_stats,  by = "to") %>%
    mutate(
      p_out     = !!w / s_out,
      p_in      = !!w / s_in,
      alpha_out = (1 - p_out)^(k_out - 1),   # Serrano 식 (출발 노드)
      alpha_in  = (1 - p_in )^(k_in  - 1),   # Serrano 식 (도착 노드)
      # OR 규칙: 양쪽 중 하나라도 유의 → backbone 보존
      keep_bb   = (alpha_out < alpha) | (alpha_in < alpha)
    ) %>%
    filter(keep_bb) %>%
    # 진단·시각화용으로 alpha_out / alpha_in 은 보존, 보조 컬럼은 제거
    select(-s_out, -k_out, -s_in, -k_in, -p_out, -p_in, -keep_bb)
}

# ---- 2b. 연도 1개에 대한 중심성 계산 함수 -----------------------------------
#  alpha = 1.0  → disparity filter 미적용 (원본 그래프)
#  alpha < 1.0  → 해당 alpha 로 backbone 추출 후 중심성 계산
compute_centrality_year <- function(yr, alpha = 1.0) {
  
  base <- df_raw %>%
    filter(
      cmdCode %in% target_hs,
      flowCode == "X",
      period == yr,
      netWgt > 0,
      netWgt > noise_floor,
      reporterISO %in% country_info$iso_3,
      partnerISO  %in% country_info$iso_3
    )
  
  if (nrow(base) == 0) {
    message(sprintf("[%d] 해당 연도 데이터 없음 ??? 건너뜀", yr))
    return(NULL)
  }
  
  # HS 합산 → 국가쌍당 1엣지
  edges_struct <- base %>%
    group_by(from = reporterISO, to = partnerISO) %>%
    summarise(net_wgt_total = sum(netWgt, na.rm = TRUE), .groups = "drop") %>%
    filter(from != to)
  
  n_edges_full <- nrow(edges_struct)
  
  # ---- Disparity Filter 적용 (alpha < 1 일 때만) -----
  if (alpha < 1) {
    edges_struct <- apply_disparity_filter(edges_struct, alpha = alpha)
    message(sprintf("[%d] disparity α=%.3f : %d → %d edges (%.1f%% 보존)",
                    yr, alpha, n_edges_full, nrow(edges_struct),
                    100 * nrow(edges_struct) / n_edges_full))
  }
  
  # betweenness 용 거리 (큰 가중치 → 짧은 거리)
  edges_struct <- edges_struct %>%
    mutate(distance = 1 / log1p(net_wgt_total))
  
  nodes <- tibble(name = unique(c(edges_struct$from, edges_struct$to))) %>%
    left_join(
      country_info %>% select(iso_3, country = name, region, sub_region),
      by = c("name" = "iso_3")
    )
  
  g <- tbl_graph(nodes = nodes, edges = edges_struct, directed = TRUE) %>%
    activate(nodes) %>%
    mutate(
      degree_out    = centrality_degree(mode = "out"),
      degree_in     = centrality_degree(mode = "in"),
      
      out_strength_raw = centrality_degree(mode = "out", weights = net_wgt_total),
      out_strength = log1p(out_strength_raw),
      
      in_strength_raw  = centrality_degree(mode = "in",  weights = net_wgt_total),
      in_strength = log1p(in_strength_raw),
      
      net_strength = out_strength - in_strength,
      
      eigen_w_raw  = centrality_eigen(directed = TRUE, weights = net_wgt_total),
      eigen_w = log1p(eigen_w_raw),
      
      # pagerank_w   = centrality_pagerank(directed = TRUE, weights = log1p(net_wgt_total)),
      pagerank_w_raw   = centrality_pagerank(directed = TRUE, weights = net_wgt_total),
      pagerank_w  = log1p(pagerank_w_raw),
      
      hub_w_raw  = centrality_hub(weights = net_wgt_total),
      hub_w  = log1p(hub_w_raw),
      
      betweenness_w = centrality_betweenness(directed = TRUE, weights = distance),
      component     = group_components(type = "weak")
    )
  
  # 연도·alpha 정보를 부착하여 long 형태로 반환
  g %>% activate(nodes) %>% as_tibble() %>%
    transmute(
      year = yr, alpha = alpha,
      name, country, region,
      degree_out, degree_in,
      out_strength, in_strength, net_strength,
      eigen_w, pagerank_w, hub_w, betweenness_w
    )
}

# ---- 3. 모든 연도 반복 -> long 패널 ----------------------------------------
centrality_panel <- map_dfr(years, compute_centrality_year,  alpha = disparity_alpha)

glimpse(centrality_panel)

# ---- 4. 저장: 통합 패널 + 연도별 개별 파일 ----------------------------------
suffix <- if (disparity_alpha < 1) sprintf("_bb_a%g", disparity_alpha) else "_full"

# write_csv(centrality_panel,
#           sprintf("centrality_panel_%d_%d%s.csv",
#                   min(years), max(years), suffix))

walk(years, function(yr) {
  sub <- centrality_panel %>% filter(year == yr)
  if (nrow(sub) > 0) {
    write_csv(sub, sprintf("centrality_%d%s.csv", yr, suffix))
  }
})

# ---- 5. 상위 N 국 추이 그래프 함수 ------------------------------------------
plot_centrality_trend <- function(panel, metric = focus_metric, top_n = 10) {
  
  top_countries <- panel %>%
    group_by(country) %>%
    summarise(score = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    slice_max(score, n = top_n, with_ties = FALSE) %>%
    pull(country)
  
  plot_df <- panel %>%
    filter(country %in% top_countries) %>%
    transmute(year, country, value = .data[[metric]]) %>%
    complete(year, country, fill = list(value = 0))   # 결측 연도는 0
  
  # 범례 정렬: 최신 연도 값 기준 내림차순
  order_lv <- plot_df %>%
    filter(year == max(year)) %>%
    arrange(desc(value)) %>%
    pull(country)
  plot_df <- plot_df %>% mutate(country = factor(country, levels = order_lv))
  
  bb_tag <- if (disparity_alpha < 1) {
    sprintf(" | Disparity α=%g backbone", disparity_alpha)
  } else { "" }
  
  ggplot(plot_df, aes(year, value, colour = country)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = sort(unique(plot_df$year))) +
    scale_colour_brewer(palette = "Paired", name = "Country") +
    labs(
      title    = sprintf("Wine Export Network ??? %s trend (Top %d)", metric, top_n),
      subtitle = sprintf("HS %s | %d???%d%s",
                         paste(target_hs, collapse = ", "),
                         min(years), max(years), bb_tag),
      x = "Year", y = metric
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
}

# ---- 6. 출력 ----------------------------------------------------------------
plot_centrality_trend(centrality_panel, metric = focus_metric, top_n = top_n)

# 다른 지표로 보고 싶을 때:
plot_centrality_trend(centrality_panel, metric = "in_strength",  top_n = 10)
plot_centrality_trend(centrality_panel, metric = "net_strength", top_n = 10)
plot_centrality_trend(centrality_panel, metric = "eigen_w",      top_n = 10)
plot_centrality_trend(centrality_panel, metric = "pagerank_w",   top_n = 10)
plot_centrality_trend(centrality_panel, metric = "hub_w",        top_n = 10)
plot_centrality_trend(centrality_panel, metric = "betweenness_w",top_n = 10)

# (선택) 여러 지표를 한 화면에 facet 으로 비교
# metrics <- c("out_strength", "pagerank_w", "eigen_w", "betweenness_w")
# top_c <- centrality_panel %>% group_by(country) %>%
#   summarise(s = mean(.data[[focus_metric]], na.rm = TRUE)) %>%
#   slice_max(s, n = top_n, with_ties = FALSE) %>% pull(country)
# centrality_panel %>%
#   filter(country %in% top_c) %>%
#   pivot_longer(all_of(metrics), names_to = "metric", values_to = "value") %>%
#   ggplot(aes(year, value, colour = country)) +
#   geom_line() + geom_point(size = 1) +
#   facet_wrap(~ metric, scales = "free_y") +
#   scale_x_continuous(breaks = years) +
#   theme_minimal()

# ---- 7. (선택) Full vs Backbone 비교 ---------------------------------------
# disparity filter 가 분석 결과에 미치는 영향을 정량 비교하려면:
#
panel_full <- map_dfr(years, compute_centrality_year, alpha = 1.0)
panel_bb05 <- map_dfr(years, compute_centrality_year, alpha = 0.05)
panel_bb01 <- map_dfr(years, compute_centrality_year, alpha = 0.01)
#
# # 상위 10개국 교집합 (out_strength 평균 기준)
top10 <- function(p) p %>% group_by(country) %>%
  summarise(s = mean(out_strength, na.rm = TRUE)) %>%
  slice_max(s, n = 10) %>% pull(country)

intersect(top10(panel_full), top10(panel_bb05))
intersect(top10(panel_full), top10(panel_bb01))

# 두 패널에서 PageRank 추이 비교
# bind_rows(
#   panel_full %>% filter(country == "Italy") %>% mutate(view = "full"),
#   panel_bb01 %>% filter(country == "Italy") %>% mutate(view = "backbone α=0.05")
# ) %>%
#   ggplot(aes(year, out_strength, colour = view)) +
#   geom_line() + geom_point() + theme_minimal()
