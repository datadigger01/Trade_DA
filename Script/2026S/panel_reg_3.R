library("tidyverse")
library("wbstats")
library("comtradr")

library(rsdmx)
library(dplyr)
library(tidyr)

# country info
url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

# IMF data
flowref <- 'IMF.STA,IMTS' 
# filter <- 'KOR.XG_FOB_USD..A'

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

kor_export <- fetch_imf_imts("KOR.XG_FOB_USD..A", "export_goods_val")
kor_import <- fetch_imf_imts(".XG_FOB_USD.KOR.A", "import_goods_val")

kor_trade_all <- kor_export %>%
  select(country, partner, year, export_goods_val) %>%
  left_join(kor_import %>% select(country, year, import_goods_val),
            by = c("partner" = "country", "year" = "year")) %>%
  mutate(import_goods_val = replace_na(import_goods_val, 1))  # 로그 변환 대비

# World Bank data
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
wdi_data <- wdi_data %>%
  mutate(
    # 1) FDI의 절대적 규모 (로그 변환을 위해, 0인 경우를 대비해 +1 처리)
    fdi_abs = abs(fdi_inflow) + 1,
    # 2) 음수 신호 더미 변수 (1: 투자 회수(음수), 0: 신규 유입 또는 0)
    fdi_neg_dummy = ifelse(fdi_inflow < 0, 1, 0)
  )


# data merge
kor_trade_all <- kor_export %>%
  select(country, partner, year, export_goods_val) %>%
  left_join(kor_import %>% select(country, year, import_goods_val),
            by = c("partner" = "country", "year" = "year")) %>%
  mutate(import_goods_val = replace_na(import_goods_val, 1))  # 로그 변환 대비

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




###############################################################################
######### Panel regression (within/first difference model) ####################
###############################################################################
library("plm")

reg_data <- kor_trade_all %>% 
  # filter(region =="Asia") %>%
  drop_na(export_goods_val, import_goods_val, gdp_per_cap, pop_t, fdi_abs, fdi_neg_dummy)
# pannel data frame 
reg_data <- pdata.frame(reg_data, index = c("partner","year"))

# reg_data <- reg_data %>%mutate(fdi_neg_dummy = relevel(as.factor(fdi_neg_dummy), ref = "0"))
plm_model <- plm(log(export_goods_val) ~  log(lag(gdp_per_cap,0:2))
                        + log(lag(export_goods_val,1))
                        + log(lag(export_goods_val,2))
                        + log(lag(import_goods_val,1))
                        + log(lag(import_goods_val,2))
                        + log(lag(fdi_abs,1)) * lag(fdi_neg_dummy,1)
                        # + log(lag(fdi_abs,2)) * lag(fdi_neg_dummy,2)
                        + log(lag(cpi,0:1))
                        + log(pop_t)
                        # + as.factor(year)
                        , data = reg_data, index=c("partner", "year"), effect='twoways', model="within")
summary(plm_model)
# cluster robust standard error
coeftest(plm_model, vcov = vcovHC(plm_model, type="HC1", cluster="group"))

# residual plot to check the panel model fit and identify any potential outliers or patterns in the residuals
resids <- as.numeric(resid(plm_model))
fitted_vals <- as.numeric(predict(plm_model))
# scatter plot
plot(resids, xlab = "Fitted Values (y-hat)", ylab = "Residuals", main = "Residuals vs Fitted (Model)",
     pch = 16, col = "black", ylim=c(-4,4))
abline(h = 0, col = "red", lwd = 2) # add baseline at y=0



#################
# housman test
#################
# formula
form <- log(export_goods_val) ~  log(lag(gdp_per_cap,0:2))
                                  + log(lag(export_goods_val,1))
                                  + log(lag(export_goods_val,2))
                                  + log(lag(import_goods_val,1))
                                  + log(lag(import_goods_val,2))
                                  + log(lag(fdi_abs,1)) * lag(fdi_neg_dummy,1)
                                  # + log(lag(fdi_abs,2)) * lag(fdi_neg_dummy,2)
                                  + log(pop_t)
fe <- plm(form, data=reg_data, model='within') # fixed effect model
re <- plm(form, data=reg_data, model='random') # random effect model
phtest(fe, re)
