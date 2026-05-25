###########################################################################################
# Practice: Building and Estimating a Gravity Model Based on Multi-country Panel Data
# Topic: Practice on Controlling Exporter/Importer Fixed Effects and PPML Estimation
##########################################################################################

# 1. package install
# install.packages(c("tidyverse", "plm", "lmtest", "sandwich", "alpaca", "fixest"))
library(tidyverse)
library(lmtest)
library(fixest) # A widely used package for fixed effects and PPML estimation in modern empirical trade analysis

# 주요 30개국 설정 (한국, 미국, 중국, 일본, 아세안 및 유럽 주요국 대리)
# countries <- c("KOR", "USA", "CHN", "JPN", "DEU", "FRA", "GBR", "VNM", "SGP", "IDN",
#                "THA", "MYS", "PHL", "AUS", "CAN", "MEX", "BRA", "IND", "RUS", "SAU",
#                "ARE", "ZAF", "ITA", "ESP", "NLD", "CHE", "TUR", "NZL", "CHL", "PER")

url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

# CEPII dataset
url1 <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/cepii_2021.csv"
gravity_df <- read_csv(url1)

# digital trade data from WTO
url2 <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/DDS_bulk_download.csv"
digital_df <- read_csv(url2)
# convert column names to lowercase
colnames(digital_df) <- tolower(colnames(digital_df))


###################################################
# 1. Gravity Data merge with Export/import dataset 
###################################################
str(gravity_df)
gravity_df1 <- gravity_df %>% filter(year >= 2000) %>% 
  select(year, country_id_o, country_id_d
         ,distw_harmonic, distw_arithmetic, distcap, dist
         ,contig
         ,comrelig, comleg_posttrans, comleg_pretrans, comcol,comlang_ethno, comlang_off
         ,sibling_ever, sibling, sib_conflict
         ,gdp_o, gdp_d, gdpcap_o, gdpcap_d
         ,eu_o,eu_d,fta_wto,fta_wto_raw,rta_coverage,rta_type
         ,entry_cost_o,entry_cost_d,entry_proc_o,entry_proc_d,entry_time_o,entry_time_d,entry_tp_o, entry_tp_d
         ,tradeflow_comtrade_o,tradeflow_comtrade_d,tradeflow_baci,manuf_tradeflow_baci,tradeflow_imf_o,tradeflow_imf_d
  )


##################################################################
## add digital trade to gravitiy data
##################################################################

# 1. add iso2 column to digital_data dataset
# For observations where indicator == "DDS" and flow == "X", replace zero values with 1 before log transformation


# 2. merge gravity data-set with digital trade data-set









##################################################################
## transform dataset for gravity model
##################################################################
gravity_multi_panel <- gravity_data %>%
      mutate(
              ln_digit_tr = log(digital_tradeflow),      # dependent variable : Y
              ln_gdp_exp = log(gdp_o),                   # log GDP of export country
              ln_gdp_imp = log(gdp_d),                  # log GDP of import country
              ln_dist = log(distw_arithmetic),          # log Distance between two countries
              pair_id = paste0(country_id_o, "_", country_id_d)  # pair id generation
              )

#--- [Model 1] 전통적 Pooled OLS (Zero 무역량 제외됨): Traditional Pooled OLS excluding zero trade flows
# 고정효과 없이 거리, GDP를 직접 추정. 다자간 저항 누락으로 인해 계수 편의(biased) 존재.

# Directly estimates the effects of distance and GDP without fixed effects.
# Coefficient estimates may be biased due to omitted multilateral resistance terms.
fit_ols <- feols(ln_digit_tr ~ ln_gdp_exp + ln_gdp_imp + ln_dist 
                 + fta_wto_raw
                 + contig 
                 + comlang_off
                 , data = gravity_multi_panel, cluster = ~pair_id)
summary(fit_ols)

#--- [Model 2] Exporter-Year & Importer-Year 고정효과 OLS (대형 패널 표준) : OLS with Exporter-Year and Importer-Year Fixed Effects
# 다자간 저항을 완벽히 흡수하므로 시간 가변 독립변수(GDP 등)는 자동으로 다중공선성 제거(Drop)됨
# 지리적 거리와 무역 비용 계수만 정밀하게 추정할 때 사용

# Absorbs multilateral resistance terms, so time-varying variables such as GDP are automatically dropped due to multicollinearity.
# Used when the objective is to precisely estimate the coefficients of geographic distance and trade costs.
fit_fe1 <- feols(ln_digit_tr ~  #ln_gdp_exp + ln_gdp_imp 
                 ln_dist
                 + fta_wto_raw
                 + contig 
                 + comlang_off | country_id_o^year + country_id_d^year
                 ,data = gravity_multi_panel, cluster = ~pair_id)
summary(fit_fe1)


fit_fe2 <- feols(ln_digit_tr ~  #ln_gdp_exp + ln_gdp_imp + ln_dist
                 fta_wto_raw
                 # + contig 
                 # + comlang_off 
                 | pair_id + country_id_o^year + country_id_d^year
                 ,data = gravity_multi_panel, cluster = ~pair_id)
summary(fit_fe2)


# compare the result of fit_fe model and the fit_ols model
etable(fit_ols, fit_fe1, fit_fe2)





# etable(fit_ols, fit_fe, order = "f", drop = "Int")
# fit_did_ppml <- feglm(tradeflow_comtrade_o ~  distw_arithmetic |
#                         pair_id + country_id_o^year + country_id_d^year,
#                       data = gravity_multi_panel, family = "poisson", cluster = ~pair_id)
# summary(fit_did_ppml)
# etable(fit_did_ppml)