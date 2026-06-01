# =============================================================================
# Wine Trade Network ??? 연도별(2021~2025) 중심성 추이
# HS 2204, 2205 / flow = Export
#
#  - 첨부 스크립트의 전처리·중심성 로직을 'compute_centrality_year()' 함수로 묶음
#  - 2021~2025 각 연도에 반복 적용 -> long 패널(centrality_panel)로 적재
#  - 연도별 CSV 저장 + 상위 N개국 중심성 추이 선그래프
# =============================================================================

library(tidyverse)
library(tidygraph)
library(scales)

# ---- 0. 설정 ----------------------------------------------------------------
url_trade   <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/wine_export_2020_2025.csv"
url_country <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"

target_hs    <- c(2204, 2205)
years        <- 2020:2025      # 분석 대상 연도
noise_floor  <- 0              # 중심성용: 0 또는 잡음 제거용 작은 값(예: 1000 kg)

focus_metric <- "out_strength" # 상위국 선정/추이 지표
#  선택지: degree_out, degree_in, out_strength,
#          in_strength, net_strength, pagerank_w, betweenness_w
top_n        <- 10

# ---- 1. 로드 (전체 연도 데이터를 한 번만 로드) ------------------------------
df_raw       <- read_csv(url_trade,   show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
country_info <- read_csv(url_country, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# ---- 2. 연도 1개에 대한 중심성 계산 함수 ------------------------------------
#  (첨부 스크립트의 base -> edges_struct -> nodes -> g -> centrality_tbl 과 동일 로직)
compute_centrality_year <- function(yr) {
  
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
  
  # 구조 엣지: HS 합산 -> 국가쌍당 1엣지 (전체 네트워크)
  edges_struct <- base %>%
    group_by(from = reporterISO, to = partnerISO) %>%
    summarise(net_wgt_total = sum(netWgt, na.rm = TRUE), .groups = "drop") %>%
    filter(from != to) %>%
    mutate(distance = 1 / log1p(net_wgt_total))
  
  nodes <- tibble(name = unique(c(edges_struct$from, edges_struct$to))) %>%
    left_join(
      country_info %>% select(iso_3, country = name, region, sub_region),
      by = c("name" = "iso_3")
    )
  
  g <- tbl_graph(nodes = nodes, edges = edges_struct, directed = TRUE) %>%
    activate(nodes) %>%
    mutate(
      degree_out   = centrality_degree(mode = "out"),
      degree_in    = centrality_degree(mode = "in"),
      # out_strength = centrality_degree(mode = "out", weights = log1p(net_wgt_total)),
      # in_strength  = centrality_degree(mode = "in",  weights = log1p(net_wgt_total)),
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
      
      component    = group_components(type = "weak")
    )
  
  # 연도 컬럼을 붙여 반환
  g %>% activate(nodes) %>% as_tibble() %>%
    transmute(
      year = yr, name, country, region,
      degree_out, degree_in,
      out_strength, in_strength, net_strength,
      eigen_w, pagerank_w,  hub_w, betweenness_w
    )
}

# ---- 3. 모든 연도 반복 -> long 패널 적재 ------------------------------------
centrality_panel <- map_dfr(years, compute_centrality_year)

glimpse(centrality_panel)

# ---- 4. 저장: 통합 패널 + 연도별 개별 파일 ----------------------------------
# write_csv(centrality_panel, "centrality_panel_2021_2025.csv")
walk(years, function(yr) {
  sub <- centrality_panel %>% filter(year == yr)
  if (nrow(sub) > 0) write_csv(sub, sprintf("centrality_%d.csv", yr))
})

# ---- 5. 상위 N개국 추이 그래프 함수 -----------------------------------------
plot_centrality_trend <- function(panel, metric = focus_metric, top_n = 10) {
  
  # 상위 N개국: 전체 연도 평균(metric) 기준
  top_countries <- panel %>%
    group_by(country) %>%
    summarise(score = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    slice_max(score, n = top_n, with_ties = FALSE) %>%
    pull(country)
  
  plot_df <- panel %>%
    filter(country %in% top_countries) %>%
    transmute(year, country, value = .data[[metric]]) %>%
    complete(year, country, fill = list(value = 0))   # 누락 연도는 0으로 채워 선 연결
  
  # 범례 정렬을 마지막 연도 값 기준 내림차순으로
  order_lv <- plot_df %>%
    filter(year == max(year)) %>%
    arrange(desc(value)) %>%
    pull(country)
  plot_df <- plot_df %>% mutate(country = factor(country, levels = order_lv))
  
  
  ggplot(plot_df, aes(year, value, colour = country)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = sort(unique(plot_df$year))) +
    scale_colour_brewer(palette = "Paired", name = "Country") +
    labs(
      title    = sprintf("Wine Export Network ??? %s trend (Top %d)", metric, top_n),
      subtitle = sprintf("HS %s | %d???%d",
                         paste(target_hs, collapse = ", "),
                         min(years), max(years)),
      x = "Year", y = metric
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
}

# ---- 6. 출력 ----------------------------------------------------------------
plot_centrality_trend(centrality_panel, metric = focus_metric, top_n = top_n)

# 다른 지표로 보고 싶을 때:
# plot_centrality_trend(centrality_panel, metric = "in_strength",  top_n = 10)
# plot_centrality_trend(centrality_panel, metric = "net_strength",  top_n = 10)

plot_centrality_trend(centrality_panel, metric = "eigen_w",    top_n = 10)
plot_centrality_trend(centrality_panel, metric = "pagerank_w",    top_n = 10)
plot_centrality_trend(centrality_panel, metric = "hub_w", top_n = 10)
# plot_centrality_trend(centrality_panel, metric = "betweenness_w", top_n = 10)

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
