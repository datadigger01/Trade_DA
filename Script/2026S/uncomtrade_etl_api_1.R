# install.packages("tidyverse")
library("tidyverse")
library("dplyr")

# install un_comtrade open api
#install.packages("comtradr")
library("comtradr")

# set primary comtrade key
set_primary_comtrade_key("41f20f9a364a4ded8849fbf33f98b01f")

# country code
# country_info <- country_codes
# str(country_info)
# # 삭제된 국가 정보는 제외
# country_info <- country_info %>% filter(is.na(exit_year)==TRUE)
url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)



# reporter_info <- ct_get_ref_table("reporter")
# str(reporter_info)


# HS 코드 정보는 상품 분류 체계로, 국제 무역에서 사용되는 표준화된 코드입니다. 
# HS 코드는 6자리로 구성되어 있으며, 무역 통계 및 관세 분류에 사용됩니다.
# get_ref_table() 함수를 이용하여 HS 코드 정보를 가져올 수 있습니다.
hs_codes <- ct_get_ref_table("HS")
str(hs_codes)

# 2-digit: 2자리 코드는 상품의 대분류를 나타냅니다. 예를 들어, "01"은 "Live animals"를 나타냅니다.
hs2 <- hs_codes %>% filter(nchar(id) == 2) %>% filter(id != "99") %>% select(id, text)

# ct_get_data() 함수를 이용하여 특정 기간 동안의 무역 데이터를 가져올 수 있습니다.
# You can request a maximum interval of twelve years from the API
hs2_data1 <- ct_get_data(
              type = "goods",
              frequency = "A",
              commodity_classification = "HS",
              # commodity_code = hs2$id,
              commodity_code = c("85"),
              flow_direction = c("export","import","re-export","re-import"),
              reporter = c("KOR"),
              partner = "all_countries",
              start_date = 2020,
              end_date = 2024,
              primary_token = get_primary_comtrade_key()
              # frequency = "M",
              # reporter = "all_countries"
              # partner = "World",
              # customs_code = "C00"
              # commodity_code = "03",
              #flow_direction = c("export","import","re-export","re-import"),
            )

hs2_data2 <- ct_get_data(
            type = "goods",
            frequency = "A",
            commodity_classification = "HS",
            # commodity_code = hs2$id,
            commodity_code = c("85"),
            flow_direction = c("export","import","re-export","re-import"),
            reporter = c("KOR"),
            partner = "all_countries",
            start_date = 2010,
            end_date = 2019,
            primary_token = get_primary_comtrade_key()
            # frequency = "M",
            # reporter = "all_countries"
            # partner = "World",
            # customs_code = "C00"
            # commodity_code = "03",
            #flow_direction = c("export","import","re-export","re-import"),
          )

# dataframe merge(결합)
hs2_data <- bind_rows(hs2_data2, hs2_data1)
str(hs2_data)

# 필요한 열만 선택하여 새로운 데이터프레임 생성
hs2_data <- hs2_data %>%
  select(c(ref_year, ref_month, reporter_iso, flow_desc, partner_iso, partner_code, 
           cmd_code, cmd_desc, qty, net_wgt, cifvalue, fobvalue, primary_value))

# Column NAME 변경
names(hs2_data) <- c("year", "month", "reporter", "flow", "partner", "partner_code",
                      "hs6", "hs6_desc", "qty", "net_wgt", "cifvalue", "fobvalue", "primary_value")

str(hs2_data)
# how to do innerjoin with country_info and hs2_data by reporter and partner
hs2_data <- hs2_data %>%
              inner_join(country_info, by = c("partner" = "iso_3")) %>% 
              select(year, reporter, flow, partner, partner_code,
                     hs6, hs6_desc, qty, net_wgt, cifvalue, fobvalue, primary_value,
                     name, region, sub_region)


# transpose : spread() 함수를 이용하여 데이터프레임을 변환
# hs2_data_w <- spread(hs2_data, key = flow, value = primary_value)
# long → wide : spread() 대체
hs2_data_w <- hs2_data %>% pivot_wider(id_cols=c("name","year","reporter","partner",
                                                 "hs6", "qty", "net_wgt"), 
                                       names_from  = flow, 
                                       values_from = primary_value)
str(hs2_data_w)

# column 순서 조정
hs2_data_w <- hs2_data_w %>% 
                relocate('reporter','partner','name','year','Export','Import') %>% 
                arrange(partner, year)

# 2024년기준으로 85(Electrical machinery and equipment and parts... 
# 수출 상위 10개 국가와 수입 상위 10개 국가 추출후 2010년 부터 2024년까지 금액단위 수출 및 수입 그래프 

# ============================================
# 1. 2024년 기준 Export 상위 10개국 추출
# ============================================
top10_countries <- hs2_data_w %>%
      filter(year == 2024, !is.na(Export)) %>%
      group_by(partner) %>%
      summarise(total_export = sum(Export, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_export)) %>%
      slice_head(n = 10)

# 상위 10개국 국가코드 벡터 추출
top10_partners <- top10_countries$partner

# ============================================
# 2. 상위 10개국의 2010-2024년 Export 추이 데이터
# ============================================
export_trend <- hs2_data_w %>%
      filter(partner %in% top10_partners,!is.na(Export)) %>%
      group_by(year, partner) %>%
      summarise(total_export = sum(Export, na.rm = TRUE), .groups = "drop")
# ============================================
# 3. ggplot 선그래프 그리기
# ============================================
ggplot(export_trend, aes(x = year, y = total_export, color = partner, group = partner)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
      labs(
        title = "2024년 수출 상위 10개국의 연도별 수출 추이 (2010-2024)",
        subtitle = "HS2 Code 85: 전기기기 및 부품",
        x = "연도",
        y = "수출액 (Billion USD)",
        color = "국가"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )


# ============================================
# 1. 2024년 기준 Import 상위 10개국 추출
# ============================================
top10_countries <- hs2_data_w %>%
    filter(year == 2024, !is.na(Import)) %>%
    group_by(partner) %>%
    summarise(total_import = sum(Import, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_import)) %>%
    slice_head(n = 10)

# 상위 10개국 국가코드 벡터 추출
top10_partners <- top10_countries$partner

# ============================================
# 2. 상위 10개국의 2010-2024년 Export 추이 데이터
# ============================================
import_trend <- hs2_data_w %>%
    filter(partner %in% top10_partners,!is.na(Import)) %>%
    group_by(year, partner) %>%
    summarise(total_import = sum(Import, na.rm = TRUE), .groups = "drop")
# ============================================
# 3. ggplot 선그래프 그리기
# ============================================
ggplot(import_trend, aes(x = year, y = total_import, color = partner, group = partner)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
    labs(
      title = "2024년 수입국가 상위 10개국의 연도별 수입액액 추이 (2010-2024)",
      subtitle = "HS2 Code 85: 전기기기 및 부품",
      x = "연도",
      y = "수입액 (Billion USD)",
      color = "국가"
    ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
  )
