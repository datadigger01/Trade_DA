#############################################################
library("tidyverse")
library("wbstats")
library("comtradr")

url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

##### IMF API with R #########################################
# install.packages("rsdmx")
# ============================================================
# Collection of Korean trade data based on IMF IMTS (2000???2025, Annual)
# ------------------------------------------------------------
# Source : IMF International Merchandise Trade Statistics
# Measure: XG_FOB_USD (Goods Export, FOB, USD)
# ============================================================

library(rsdmx)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1) IMF IMTS fetch
#    key 포맷: "REPORTER.INDICATOR.PARTNER.FREQ"
#      "KOR.XG_FOB_USD..A" → 한국의 對파트너 수출
#      ".XG_FOB_USD.KOR.A" → 파트너의 對한국 수출 (= 한국 수입)
# ------------------------------------------------------------
# agency and id identify the dataset you are interested in
flowref <- 'IMF.STA,IMTS' # STA: International Financial Statistics (IFS), ITG: International Trade in Goods and Services
# IMTS: International Merchandise Trade Statistics (IMTS)

# filter identifies the subset of the dataset you want.
filter <- 'KOR.XG_FOB_USD..A' # XG: export of goods, FOB value, USD; A: annual data
# .XG_FOB_USD.KOR.A

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

# ------------------------------------------------------------
# 2) 수출·수입 데이터 수집
# ------------------------------------------------------------
kor_export <- fetch_imf_imts("KOR.XG_FOB_USD..A", "export_goods_val")
kor_import <- fetch_imf_imts("KOR.MG_CIF_USD..A", "import_goods_val")

# ------------------------------------------------------------
# 3) 결합 (파트너 × 연도 기준)
#    kor_import의 country = 한국의 교역상대국 → partner로 매칭
# ------------------------------------------------------------
kor_trade_all <- kor_export %>%
  select(country, partner, year, export_goods_val) %>%
  left_join(kor_import %>% select(country, partner, year, import_goods_val),
            by = c('country'='country', "partner" = "partner", "year" = "year")) %>%
  mutate(import_goods_val = replace_na(import_goods_val, 1))  # 로그 변환 대비
                                
############################################
### World Bank Open Data API : wbstats
############################################
wdi_trade <- wb_search(pattern = "export")
wdi_trade <- wb_search(pattern = "import")
my_indicators <- c( "export_val_kr"  = "NE.EXP.GNFS.KN"
                   ,"export_val_usd" = "NE.EXP.GNFS.KD"
                   ,"import_val_kr"  = "NE.IMP.GNFS.KN"
                   ,"import_val_usd" = "NE.IMP.GNFS.KD"
                   ,"gdp_val"        = "NY.GDP.MKTP.KD"
                   ,"gdp_growth_r"   = "NY.GDP.MKTP.KD.ZG"
                   ,"gdp_per_cap"    = "NY.GDP.PCAP.KD"
                   ,"cpi"            = "FP.CPI.TOTL"
                   ,"pop_t"          = "SP.POP.TOTL"
                   ,"pop_1564"       = "SP.POP.1564.TO"
                   ,"fdi_r_gdp"      = "BX.KLT.DINV.WD.GD.ZS"
                   ,"fdi_inflow"     = "BX.KLT.DINV.CD.WD"
)
wdi_data <- wb_data(my_indicators, country = "all", start_date = 2000, end_date = 2025)

#######################################
####### 변수 변환 #####################
#######################################
# 1. IHS 변환 함수 정의
# ihs <- function(x) {x + sqrt(x^2 + 1)}
wdi_data <- wdi_data %>%
  mutate(
    # 1) FDI의 절대적 규모 (로그 변환을 위해, 0인 경우를 대비해 +1 처리)
    fdi_abs = abs(fdi_inflow) + 1,
    # 2) 음수 신호 더미 변수 (1: 투자 회수(음수), 0: 신규 유입 또는 0)
    fdi_neg_dummy = ifelse(fdi_inflow < 0, 1, 0)
  )



# merge the IMF export data with the target ASEAN countries data
kor_trade_all <- kor_trade_all %>%
                      filter(country == "KOR") %>% 
                      inner_join(country_info, by = c("partner" = "iso_3")) %>% 
                      select(country, partner, name, region, sub_region, year, export_goods_val, import_goods_val)

# merge the export data with the World Bank indicators data
kor_trade_all <- kor_trade_all %>% 
                     inner_join(wdi_data, by = c("partner" = "iso3c", "year" = "date")) %>% 
                     select(country=country.x, partner, name, region, sub_region, year, 
                            export_goods_val, import_goods_val, export_val_kr, export_val_usd, import_val_kr, import_val_usd,
                            gdp_val, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564, fdi_r_gdp, fdi_inflow, fdi_abs, fdi_neg_dummy)
                     

###### visualization #############
# unique(kor_trade_all$sub_region)
# # options(scipen = 999)
kor_trade_all %>%
  # filter(region =="Asia") %>%
  drop_na(fdi_abs, export_goods_val) %>%
  ggplot(mapping = aes(x=log(lag(import_goods_val)), y=log(export_goods_val)
                       # ,color=as.factor(year)
                       )
         ) +
  geom_point() +
  # facet_grid(~year) +
  geom_smooth(method = "lm", se=F) +
  theme_bw()


###############################################################################
######### Panel regression (within model) ####################################
##############################################################################
library("plm")

reg_data <- kor_trade_all %>% 
  # filter(sub_region =="South-eastern Asia") %>% 
  drop_na(export_goods_val, import_goods_val, gdp_per_cap, pop_t, fdi_abs, fdi_neg_dummy)
reg_data <- pdata.frame(reg_data, index = c("partner","year"))  
  
plm_model_within <- plm(log(export_goods_val) ~ gdp_growth_r + lag(gdp_growth_r,1) + lag(gdp_growth_r,2) +
                                          + log(lag(export_goods_val,1))
                                          + log(lag(export_goods_val,2))
                                          + log(lag(import_goods_val,1))
                                          + log(lag(import_goods_val,2))
                                          + log(lag(cpi,0:2))
                                          + log(pop_t)
                                          # + as.factor(year)
                 , data = reg_data, index=c("partner", "year"), effect = 'twoways', model="within")
summary(plm_model_within)

# residual plot to check the panel model fit and identify any potential outliers or patterns in the residuals
resids <- as.numeric(resid(plm_model_within))
fitted_vals <- as.numeric(predict(plm_model_within))
# scatter plot
plot(resids, xlab = "Fitted Values (y-hat)", ylab = "Residuals", main = "Residuals vs Fitted (Within Model)",
     pch = 16, col = "black", ylim=c(-4,4))
abline(h = 0, col = "red", lwd = 2) # add baseline at y=0



#####################################################################
# residual distribution (normality check)
#####################################################################
resid_df <- data.frame(residuals = as.numeric(resid(plm_model_within)))
ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "lightgray", color = "white") +
  geom_density(color = "black", linewidth = 1) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(resid_df$residuals),sd   = sd(resid_df$residuals)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "blue", linewidth = 0.8, linetype = "dotted") +
  labs(title    = "Distribution of Within Model Residuals", 
       subtitle = "Black: empirical density | Red dashed: normal distribution",
       x        = "Residuals",
       y        = "Density") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5))
