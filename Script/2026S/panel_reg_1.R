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
kor_import <- fetch_imf_imts(".XG_FOB_USD.KOR.A", "import_goods_val")

# ------------------------------------------------------------
# 3) 결합 (파트너 × 연도 기준)
#    kor_import의 country = 한국의 교역상대국 → partner로 매칭
# ------------------------------------------------------------
kor_trade_all <- kor_export %>%
  select(country, partner, year, export_goods_val) %>%
  left_join(kor_import %>% select(country, year, import_goods_val),
            by = c("partner" = "country", "year" = "year")) %>%
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
                            gdp_val, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564, fdi_r_gdp, fdi_inflow)
                     






unique(kor_trade_all$sub_region)
# options(scipen = 999)
kor_trade_all %>% 
  # filter(year >= 2015) %>% 
  filter(sub_region =="South-eastern Asia") %>% 
  drop_na(gdp_per_cap, export_goods_val) %>% 
  ggplot(mapping = aes(x=log(lag(gdp_per_cap)), y=log(export_goods_val))) +
  geom_point() +
  # facet_grid(~year) +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

## OLS regression with lagged independent variables to check the relationship between export value and GDP value, controlling for other factors
reg_model<- lm(log(export_goods_val) ~  log(lag(gdp_per_cap))
               # + log(lag(import_goods_val))
               # + log(lag(cpi,1))
               # + log(lag(pop_t,1))
               , data = reg_data)
summary(reg_model)
# residual plot to check the model fit and identify any potential outliers or patterns in the residuals
plot(reg_model$residuals, pch=16, col="black", main="Residuals of OLS Regression", xlab="Index", ylab="Residuals", ylim=c(-4,4))
abline(h=0, col="red", lwd=2)

# residual distribution (normality check)
#######################################################
resid_df <- data.frame(residuals = reg_model$residuals)
ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgray", color = "white") +
  geom_density(color = "black", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(resid_df$residuals), sd   = sd(resid_df$residuals)),color = "red", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "blue", linewidth = 0.8, linetype = "dotted") +
  labs(title = "Distribution of OLS Residuals",
       subtitle = "Black: empirical density | Red dashed: normal distribution", x = "Residuals", y = "Density") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),plot.subtitle = element_text(hjust = 0.5))




## panel regression model approach with gdp_per_capita (demeaning / centering)
###############################################################################
str(kor_trade_all)
reg_data <- kor_trade_all %>%
            filter(sub_region =="South-eastern Asia") %>% 
            # drop_na(export_goods_val, gdp_per_cap, cpi, fdi_r_gdp, fdi_inflow, pop_t)
            group_by(partner) %>% 
            mutate(
                avglog_gdppercap         = mean(log(gdp_per_cap), na.rm=TRUE),
                avglog_export_goods_val  = mean(log(export_goods_val), na.rm=TRUE),
                dif_loggdppercap         = log(gdp_per_cap)        - avglog_gdppercap,
                dif_logexport_goods_val  = log(export_goods_val)   - avglog_export_goods_val
            ) %>% ungroup() %>% 
            drop_na(dif_loggdppercap, dif_logexport_goods_val) %>% 
            select(country, partner, name, region, sub_region, year, 
                   export_goods_val,import_goods_val,gdp_per_cap, cpi, pop_t, fdi_r_gdp, fdi_inflow, gdp_growth_r,
                   avglog_gdppercap, avglog_export_goods_val, dif_logexport_goods_val,dif_loggdppercap)

reg_data %>% 
  ggplot(mapping = aes(x=lag(dif_loggdppercap), y=dif_logexport_goods_val)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

## panel regression with lagged independent variables to check the relationship between export value and GDP value, controlling for other factors
panelreg_model<- lm(dif_logexport_goods_val ~  lag(dif_loggdppercap)
                    , data = reg_data)
summary(panelreg_model)
# residual plot to check the model fit and identify any potential outliers or patterns in the residuals
plot(panelreg_model$residuals, pch=16, col="black", main="Residuals of OLS Regression", xlab="Index", ylab="Residuals", ylim=c(-4,4))
abline(h=0, col="red", lwd=2)

# residual distribution (normality check)
resid_df1 <- data.frame(residuals = panelreg_model$residuals)
ggplot(resid_df1, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgray", color = "white") +
  geom_density(color = "black", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(resid_df1$residuals), sd   = sd(resid_df1$residuals)),color = "red", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "blue", linewidth = 0.8, linetype = "dotted") +
  labs(title = "Distribution of Panel Regression Residuals",
       subtitle = "Black: empirical density | Red dashed: normal distribution", x = "Residuals", y = "Density") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),plot.subtitle = element_text(hjust = 0.5))



######### Panel regression (within model) #####################################
################################################################################
library("plm")

str(reg_data)
plm_model <- plm(log(export_goods_val) ~  log(lag(gdp_per_cap))
                                          # + log(lag(export_goods_val,1:2))
                                          # + log(lag(import_goods_val,1:2))
                                          # + log(pop_t)
                                          # + as.factor(year)
                 , data = reg_data, index=c("partner", "year"), model="within")
summary(plm_model)

# residual plot to check the panel model fit and identify any potential outliers or patterns in the residuals
resids <- as.numeric(resid(plm_model))
fitted_vals <- as.numeric(predict(plm_model))
# scatter plot
plot(resids,
     xlab = "Fitted Values (y-hat)",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Within Model)",
     pch = 16, col = "black", ylim=c(-4,4))
abline(h = 0, col = "red", lwd = 2) # add baseline at y=0


# residual distribution (normality check)
resid_df <- data.frame(residuals = as.numeric(resid(plm_model)))
ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgray", color = "white") +
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
