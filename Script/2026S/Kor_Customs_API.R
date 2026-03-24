# ============================================
# Library 로드
# ============================================
library(httr)
library(xml2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ============================================
# API 설정
# ============================================
Servicekey <- "##############################"
url <- "http://apis.data.go.kr/1220000/nitemtrade/getNitemtradeList"

# ============================================
# 연도별 API 호출 및 데이터 수집
# ============================================
# HS코드 목록 정의
hs_codes <- c("2008995010", "1212211010", "2106904010")  # HScode : 김
# hs_codes <- c("1902301010")  # HScode : 라면

all_trade_data <- list()

for (hscode in hs_codes) {
  for (year in 2015:2026) {
    strtYymm <- paste0(year, "01")
    endYymm  <- paste0(year, "12")
    
    # API 요청
    response <- GET(
      url = url,
      query = list(
        serviceKey = Servicekey,
        strtYymm   = strtYymm,
        endYymm    = endYymm,
        hsSgn      = hscode
      )
    )
    
    # 응답 상태 확인
    cat(sprintf("HS: %s | Year: %d | Status: %d\n", hscode, year, status_code(response)))
    
    # XML 파싱
    contents <- content(response, as = "text", encoding = "UTF-8")
    xml_doc  <- read_xml(contents)
    
    # item 노드 추출
    items <- xml_find_all(xml_doc, "//item")
    
    if (length(items) > 0) {
      row_list <- lapply(items, function(item) {
        children <- xml_children(item)
        values   <- xml_text(children)
        names(values) <- xml_name(children)
        as.data.frame(t(values), stringsAsFactors = FALSE)
      })
      
      trade_df <- bind_rows(row_list) #%>%
        # mutate(hsSgn = hscode)  # HS코드 컬럼 추가
      
      # key : "hscode_year" 형태로 저장
      key <- paste0(hscode, "_", year)
      all_trade_data[[key]] <- trade_df
      cat(sprintf("Data retrieved → HS: %s | Year: %d | Rows: %d\n", hscode, year, nrow(trade_df)))
      
    } else {
      cat(sprintf("No data → HS: %s | Year: %d\n", hscode, year))
    }
    
    Sys.sleep(0.3)  # API 과부하 방지
  }
}

# 전체 데이터 하나로 합치기
full_trade_df <- bind_rows(all_trade_data)

# ============================================
# 데이터 구조 확인
# ============================================
str(full_trade_df)

# ============================================
# 데이터 타입 변환
# ============================================
# 정수형으로 변환할 컬럼
cols_to_int <- c("balPayments", "expDlr", "expWgt", "impDlr", "impWgt")

# cols_to_int 대상 컬럼에 대해서 숫자형 변환
for (col in cols_to_int) {
  full_trade_df[[col]] <- as.numeric(full_trade_df[[col]])
}

# year 컬럼: 앞 4자리 년도만 추출
library(lubridate)
full_trade_df <- full_trade_df %>%
  mutate(yymm = ym(gsub("\\.", "", year)))  # "2020.10" → "202010" → Date 변환

# 결측값 제거
full_trade_df <- full_trade_df %>%
  filter(!is.na(yymm)) %>%
  filter(if_all(all_of(cols_to_int), ~ !is.na(.)))

# 정수형으로 최종 변환
for (col in cols_to_int) {
  full_trade_df[[col]] <- as.integer(full_trade_df[[col]])
}
#full_trade_df$year <- as.integer(full_trade_df$year)

print(head(full_trade_df, 10))
str(full_trade_df)




### country info
###################################################################################################
url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)
str(country_info)

# name,region, sub_region 정보 추가
full_trade_df <- full_trade_df %>%
  inner_join(country_info %>% select(name,iso_2, iso_3, region, sub_region),  # 필요한 컬럼만 선택
            by = c("statCd" = "iso_2"))

# ============================================
# 그룹화 및 집계 (statCd, year 기준)
# ============================================
trade_df_yearbase <- full_trade_df %>%
  group_by(year = year(yymm), name) %>%
  summarise(
    balPayments = sum(balPayments, na.rm = TRUE),
    expDlr = sum(expDlr, na.rm = TRUE),
    expWgt = sum(expWgt, na.rm = TRUE),
    impDlr = sum(impDlr, na.rm = TRUE),
    impWgt = sum(impWgt, na.rm = TRUE),
    
    statCdCntnKor1 = first(statCdCntnKor1),  
    # statKor        = first(statKor),           
    name           = first(name),
    iso3          = first(iso_3),
    region         = first(region),            
    sub_region     = first(sub_region),        
    .groups = "drop"
  ) %>% filter(year >= 2015 & year <= 2025)

str(trade_df_yearbase)
# ============================================
# 2020년기준 expWgt 백분위수 정보
# ============================================
trade_2020_df <- trade_df_yearbase %>% filter(year == 2020)
expwgt_percentiles <- summary(trade_2020_df$expWgt)
print(expwgt_percentiles)
# 상세 백분위수
quantile(trade_2020_df$expWgt, probs = c(0, 0.25, 0.5, 0.75, 1))


# ============================================
# 2020년 기준 expWgt >= 3000 필터링 및 병합
# ============================================
target_list <- trade_df_yearbase %>%
  filter(year == 2020, expWgt >= 4000) %>%
  select(year, name, expWgt) %>%
  rename(year_target = year, expWgt_target = expWgt)


# 병합
merged_df <- trade_df_yearbase %>%
  inner_join(target_list, by = "name") 


# ============================================
# 비율 계산 (2020년 기준)
# ============================================
merged_df <- merged_df %>%
  mutate(ratio_expWgt_2020b = ifelse(
                                is.na(expWgt / expWgt_target), 0, 
                                expWgt / expWgt_target
                                )
        ) 

# =================================================
# 선그래프 시각화 - 전체대상 2020기준 수출량 비율 추이
# ================================================
mean_ratio <- merged_df %>%
  group_by(year) %>%
  summarise(mean_ratio = mean(ratio_expWgt_2020b, na.rm = TRUE), .groups = "drop")

# 그래프 그리기
ggplot() +
  # 개별 국가 라인 (투명하게)
  geom_line(
    data = merged_df,
    aes(x = year, y = ratio_expWgt_2020b, group = name, color = name),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  # 평균 라인 (굵게)
  geom_line(
    data = mean_ratio,
    aes(x = year, y = mean_ratio),
    color = "black",
    linewidth = 1.5
  ) +
  # 평균 포인트
  geom_point(
    data = mean_ratio,
    aes(x = year, y = mean_ratio),
    color = "black",
    size = 2
  ) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(
    title = "Ratio of Export Weight (2020 base) over Time by Country",
    x = "Year",
    y = "Ratio (Export Weight / 2020 Export Weight)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )


# 2025년기준 상위 10개국의 수출량 비율 추이 그래프
# ============================================
# 1. 2025년 기준 Export 상위 10개국 추출
# ============================================
top10_countries <- trade_df_yearbase %>%
  filter(year == 2025, !is.na(expWgt)) %>%
  group_by(name) %>%
  summarise(total_export = sum(expWgt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_export)) %>%
  slice_head(n = 10)

str(top10_countries)

# 상위 10개국 국가코드 벡터 추출
top10_partners <- top10_countries$name
# ============================================
# 1. 상위 10개국의 2015-2025년 Export 년별추이
# ============================================
export_top10 <- merged_df %>%
          filter(name %in% top10_partners)



# ============================================
# 2. 상위 top10 수출량 년도별 추이이
# ============================================
# 연도별 평균 계산
mean_top10 <- export_top10 %>%
  group_by(year) %>%
  summarise(mean = mean(expWgt, na.rm = TRUE), .groups = "drop")
ggplot() +
  geom_line(
    data = export_top10,
    aes(x = year, y = expWgt, group = name, color = name),
    linewidth = 0.6,
    alpha = 0.8,
    show.legend = TRUE
  ) +
  geom_line(
    data = mean_top10,
    aes(x = year, y = mean),
    color = "black",
    linewidth = 1.2,
    linetype ='dashed'
  ) +
  geom_point(
    data = mean_top10,
    aes(x = year, y = mean),
    color = "black",
    size = 2
  ) +
  # 오른쪽 끝 라벨 추가
  geom_text(
    data = export_top10 %>% filter(year == max(year)),
    aes(x = year, y = expWgt, label = name, color = name),
    hjust = -0.2,   # 라인 오른쪽으로 간격 조정
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = 2015:2025,
    expand = expansion(mult = c(0.05, 0.15))  # 오른쪽 여백 확보
  ) +
  labs(
    title = "Top10 Export Weight over Time by Country",
    x = "Year",
    y = "Export Weight"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ============================================
# 2. 2020년 기준 수출량 비율 년도별 추이
# ============================================
# 연도별 평균 계산
mean_ratio_top10 <- export_top10 %>%
  group_by(year) %>%
  summarise(mean_ratio = mean(ratio_expWgt_2020b, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(
    data = export_top10,
    aes(x = year, y = ratio_expWgt_2020b, group = name, color = name),
    linewidth = 0.8,
    alpha = 0.8,
    show.legend = TRUE
  ) +
  geom_line(
    data = mean_ratio_top10,
    aes(x = year, y = mean_ratio),
    color = "black",
    linewidth = 1.2,
    linetype ='dashed'
  ) +
  geom_point(
    data = mean_ratio_top10,
    aes(x = year, y = mean_ratio),
    color = "black",
    size = 2
  ) +
  # 오른쪽 끝 라벨 추가
  geom_text(
    data = export_top10 %>% filter(year == max(year)),
    aes(x = year, y = ratio_expWgt_2020b, label = name, color = name),
    hjust = -0.2,   # 라인 오른쪽으로 간격 조정
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = 2015:2025,
    expand = expansion(mult = c(0.05, 0.15))  # 오른쪽 여백 확보
  ) +
  labs(
    title = "Top10 Export Weight Ratio over Time by Country",
    x = "Year",
    y = "Ratio (Export Weight / 2020 Export Weight)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )






# =============================================================
# . 상위 3개국의 2016-2025년 Export 월별추이 데이터(금액기준준)
# =============================================================
top3_countries <- trade_df_yearbase %>%
  filter(year == 2025, !is.na(expWgt)) %>%
  group_by(name) %>%
  summarise(total_export = sum(expWgt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_export)) %>%
  slice_head(n = 3)
# 상위 3개국 국가코드 벡터 추출
top3_partners <- top3_countries$name
# ============================================
# 1. 상위 3개국의 2015-2025년 Export 년별추이
# ============================================
export_top3 <- full_trade_df %>%
  filter(name %in% top3_partners) %>% 
  group_by(yymm, name) %>% 
  summarise(total_export_wgt = sum(expWgt, na.rm=TRUE), .groups = "drop")

ggplot() +
  geom_line(
    data = export_top3,
    aes(x = yymm, y = total_export_wgt, group = name, color = name),
    linewidth = 0.8,
    alpha = 0.8,
    show.legend = TRUE
  ) +
  geom_text(
    data = export_top3 %>% filter(yymm == max(yymm)),
    aes(x = yymm, y = total_export_wgt, label = name, color = name),
    hjust = -0.2,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_date(
    date_breaks = "6 months",       # 6개월 간격으로 표시
    date_labels = "%Y-%m",          # 년-월 형식으로 표시
    expand = expansion(mult = c(0.05, 0.1))  # 오른쪽 여백 확보
  ) +
  labs(
    title = "Export Weight over Time by Country",
    x = "Year",
    y = "Export Weight, Monthly"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)  # x축 라벨 45도 회전
  )
