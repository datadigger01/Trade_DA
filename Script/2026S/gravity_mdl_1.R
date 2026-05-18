###########################################################################################
# Practice: Building and Estimating a Gravity Model Based on Multi-country Panel Data
# Topic: Practice on Controlling Exporter/Importer Fixed Effects and PPML Estimation
##########################################################################################

# 1. package install
install.packages(c("tidyverse", "plm", "lmtest", "sandwich", "alpaca", "fixest"))
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(fixest) # A widely used package for fixed effects and PPML estimation in modern empirical trade analysis

# 주요 30개국 설정 (한국, 미국, 중국, 일본, 아세안 및 유럽 주요국 대리)
countries <- c("KOR", "USA", "CHN", "JPN", "DEU", "FRA", "GBR", "VNM", "SGP", "IDN",
               "THA", "MYS", "PHL", "AUS", "CAN", "MEX", "BRA", "IND", "RUS", "SAU",
               "ARE", "ZAF", "ITA", "ESP", "NLD", "CHE", "TUR", "NZL", "CHL", "PER")

# tr_gravity_data <- readRDS("D:/Data/Gravity_V202211.rds")
# target_gr <- tr_gravity_data %>% filter ( country_id_o %in% countries )
# target_gr <- target_gr %>% filter( country_id_d %in% country_id_o & country_id_o != country_id_d & year >= 2000 )

# CEPII dataset
url1 <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/cepii_2021.csv"
gravity_df <- read_csv(url1)

# IMF database
##################################################################################################
library(rsdmx)
library(dplyr)
library(purrr)
library(tidyr)

fetch_imf_imts <- function(filter_key,
                           value_name,
                           start   = 2000,
                           end     = 2025,
                           flowref = "IMF.STA,IMTS") {
  readSDMX(providerId = "IMF_DATA",
           resource   = "data",
           flowRef    = flowref,
           key        = filter_key,
           start      = start,
           end        = end) %>% 
    as.data.frame() %>% 
    rename_with(tolower) %>% 
    transmute(country,
              partner   = counterpart_country,
              indicator,
              frequency,
              year             = as.numeric(time_period),
              !!value_name    := as.numeric(obs_value))
}
# 2) target countries
countries <- c("KOR", "USA", "CHN", "JPN", "DEU", "FRA", "GBR", "VNM", "SGP", "IDN",
               "THA", "MYS", "PHL", "AUS", "CAN", "MEX", "BRA", "IND", "RUS", "SAU",
               "ARE", "ZAF", "ITA", "ESP", "NLD", "CHE", "TUR", "NZL", "CHL", "PER")
# 3) 다국가 래퍼: 국가코드를 받아 key를 조립 → 호출
#    실패에 안전하도록 safely()로 감싸고, 진행상황을 보고 싶으면 .progress = TRUE
fetch_imf_imts_multi <- function(countries,
                                 indicator   = "XG_FOB_USD",   # Export=XG_FOB_USD, Import=MG_CIF_USD 등
                                 value_name  = "export_goods_val",
                                 start       = 2000,
                                 end         = 2025,
                                 delay       = 0.3) {
  
  safe_fetch <- safely(fetch_imf_imts, otherwise = NULL)
  
  results <- map(countries, function(ctry) {
    key <- paste0(ctry, ".", indicator, "..A")    # ex: "USA.XG_FOB_USD..A"
    message("Fetching: ", ctry)
    res <- safe_fetch(filter_key = key,
                      value_name = value_name,
                      start      = start,
                      end        = end)
    Sys.sleep(delay)    # API 부하 완화
    res$result          # 성공시 데이터, 실패시 NULL
  })
  
  # 실패한 국가 알림
  failed <- countries[map_lgl(results, is.null)]
  if (length(failed) > 0) {
    warning("failed country: ", paste(failed, collapse = ", "))
  }
  
  bind_rows(results)
}
# 4) 사용
target_export <- fetch_imf_imts_multi(
  countries  = countries,
  indicator  = "XG_FOB_USD",
  value_name = "export_goods_val"
)

# # 수입도 같은 방식으로
# target_import <- fetch_imf_imts_multi(
#   countries  = countries,
#   indicator  = "MG_CIF_USD",
#   value_name = "import_goods_val"
# )
#################################################################################################

##################################################################
## Example dataset for gravity model
##################################################################
gravity_data <- gravity_multi_panel %>%
  mutate(
    ln_export = log(export_goods_val),    # dependent variable : Y
    ln_gdp_exp = log(gdp_exp),            # log GDP of export country
    ln_gdp_imp = log(gdp_imp),            # log GDP of import country
    ln_dist = log(distance),              # log Distance between two countries
    pair_id = paste0(exporter, "_", importer)  # pair id generation
  )

#--- [Model 1] 전통적 Pooled OLS (Zero 무역량 제외됨)
# 고정효과 없이 거리, GDP를 직접 추정. 다자간 저항 누락으로 인해 계수 편의 존재.
fit_ols <- feols(ln_trade ~ ln_gdp_exp + ln_gdp_imp + ln_dist + contiguity + comlang, 
                 data = gravity_data, cluster = ~pair_id)

#--- [Model 2] Exporter-Year & Importer-Year 고정효과 OLS (대형 패널 표준)
# 다자간 저항을 완벽히 흡수하므로 시간 가변 독립변수(GDP 등)는 자동으로 다중공선성 제거(Drop)됨
# 지리적 거리와 무역 비용 계수만 정밀하게 추정할 때 사용
fit_fe <- feols(ln_trade ~ ln_dist + contiguity + comlang | exporter^year + importer^year, 
                data = gravity_data, cluster = ~pair_id)

