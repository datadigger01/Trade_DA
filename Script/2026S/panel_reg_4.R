library("tidyverse")
library("wbstats")
library("comtradr")

library(rsdmx)
library(dplyr)
library(tidyr)

################################################################################################
# 1. data preparation for panel data regression model
################################################################################################
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

kor_export <- fetch_imf_imts("KOR.XG_FOB_USD..A", "export_goods_val")  # Korea good export
kor_import <- fetch_imf_imts("KOR.MG_CIF_USD..A", "import_goods_val")  # Korea good import

# merge Korea goods export dataset with Korea goods import dataset
kor_trade_all <- kor_export %>%
  select(country, partner, year, export_goods_val) %>%
  left_join(kor_import %>% select(country, partner, year, import_goods_val),
            by = c('country'='country', "partner" = "partner", "year" = "year")) %>%
  mutate(import_goods_val = replace_na(import_goods_val, 1))  # replace NA or zero value with 1 for before applying log transformation

# World Bank data
wdi_indicator <- wb_search(pattern = "development")
my_indicators <- c( "export_val_kr"   = "NE.EXP.GNFS.KN"
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
    # 1) Absolute magnitude of FDI :Add 1 to allow log transformation and handle zero values
    fdi_abs = abs(fdi_inflow) + 1,
    # 2) Dummy variable indicating negative FDI inflows : withdrawal of investment; 0: New inflow or zero
    fdi_neg_dummy = ifelse(fdi_inflow < 0, 1, 0)
  )

# merge the IMF export data with the target countries data
kor_trade_all_1 <- kor_trade_all %>%
  filter(country == "KOR") %>% 
  inner_join(country_info, by = c("partner" = "iso_3")) %>% 
  select(country, partner, name, region, sub_region, year, export_goods_val, import_goods_val)

# merge the export data with the World Bank indicators data
kor_trade_all_f <- kor_trade_all_1 %>% 
  inner_join(wdi_data, by = c("partner" = "iso3c", "year" = "date")) %>% 
  select(country=country.x, partner, name, region, sub_region, year, 
         export_goods_val, import_goods_val, export_val_kr, export_val_usd, import_val_kr, import_val_usd,
         gdp_val, gdp_growth_r, gdp_per_cap, 
         cpi, pop_t, pop_1564, fdi_r_gdp, fdi_inflow, fdi_abs, fdi_neg_dummy)
#################################################################################################################



###############################################################################
# 2. Panel regression (within/fd:first difference model) ######################
###############################################################################
# install.packages(c("plm","lmtest"))
library("plm")
library("lmtest")

reg_data <- kor_trade_all_f %>% 
  # filter(sub_region=='Southern Asia') %>% 
  drop_na(export_goods_val, import_goods_val, gdp_per_cap, pop_t, fdi_abs, fdi_neg_dummy) %>% 
  group_by(partner) %>%
  mutate(
        ln_export  = log(export_goods_val), # natural log transformation of export_goods_val
        ln_import  = log(import_goods_val), # natural log transformation of import_goods_val
        ln_gdp_pc  = log(gdp_per_cap),      # natural log transformation of gdp_per_cap
        ln_fdi_abs = log(fdi_abs),          # natural log transformation of fdi_abs
        ln_cpi     = log(cpi),              # natural log transformation of cpi
        ln_pop     = log(pop_t)             # natural log transformation of pop_t
        ) %>%
  ungroup()

# panel data frame 
reg_data_plm <- pdata.frame(reg_data, index = c("partner","year"))

# reg_data <- reg_data %>%mutate(fdi_neg_dummy = relevel(as.factor(fdi_neg_dummy), ref = "0"))
plm_model <- plm( ln_export ~  lag(ln_gdp_pc,0) + lag(ln_gdp_pc,1)
                                # + lag(ln_export,1) + lag(ln_export,2)
                               + lag(ln_import,1) + lag(ln_import,2)
                               + lag(ln_cpi,0) + lag(ln_cpi,1) + lag(ln_cpi,2)
                               + lag(ln_fdi_abs,1) * lag(fdi_neg_dummy,1)
                               # + lag(ln_fdi_abs,2) * lag(fdi_neg_dummy,2)
                               + ln_pop
                               + as.factor(year)
                  ,data = reg_data_plm, index=c("partner", "year"), effect='individual', model="within")
# output summary
summary(plm_model)

# cluster robust standard error
coeftest(plm_model, vcov = vcovHC(plm_model, type = "HC1", cluster = "group"))


# residual plot to check the panel model fit and identify any potential outliers or patterns in the residuals
resids <- as.numeric(resid(plm_model))
fitted_vals <- as.numeric(predict(plm_model))
# scatter plot
plot(resids, xlab = "Fitted Values (y-hat)", ylab = "Residuals", main = "Residuals vs Fitted (Within Model)",
     pch = 16, col = "black", ylim=c(-4,4))
abline(h = 0, col = "red", lwd = 2) # add baseline at y=0


######### Panel regression (LSDV: Least Square Dummy Variable) ################
###############################################################################
lsdv_model <- plm( ln_export ~  lag(ln_gdp_pc,0) + lag(ln_gdp_pc,1)
                                + lag(ln_import,1) + lag(ln_import,2)
                                + lag(ln_cpi,0) + lag(ln_cpi,1) + lag(ln_cpi,2)
                                + lag(ln_fdi_abs,1) * lag(fdi_neg_dummy,1)
                                # + lag(ln_fdi_abs,2) * lag(fdi_neg_dummy,2)
                                + ln_pop
                                + as.factor(partner)
                                + as.factor(year)
                                ,data=reg_data_plm, model='pooling')
summary(lsdv_model)
# cluster robust standard error
coeftest(lsdv_model, vcov = vcovHC(lsdv_model, type = "HC1", cluster = "group"))

##########################################################################
# stargazer output
###########################################################################
# library(sandwich)
# library(stargazer)
# # 2. Cluster robust variance-covariance matrix & coeftest
# cl_vcov   <- vcovHC(lsdv_model, type = "HC1", cluster = "group")
# cl_test   <- coeftest(lsdv_model, vcov = cl_vcov)
# # 3. Cluster SE & p-value
# cl_se <- cl_test[, "Std. Error"]
# cl_p  <- cl_test[, "Pr(>|t|)"]
# # 4. stargazer output
# stargazer(
#   lsdv_model,
#   se   = list(cl_se),
#   p    = list(cl_p),
#   type = "text",                       # 콘솔 확인용. 논문용은 "latex", 워드용은 "html"
#   title = "LSDV Estimation with Cluster-Robust SE (HC1, clustered by partner)",
#   dep.var.labels = "ln(Export)",
#   omit = c("partner", "year"),    # partner/year 더미는 표에서 숨김
#   omit.labels = c("Partner FE", "Year FE"),
#   omit.stat = c("f", "ser"),           # 필요 없으면 통계량 일부 제거
#   digits = 4,
#   notes  = "Cluster-robust standard errors (by partner) in parentheses.",
#   notes.align = "l"
# )
