# installation un_comtrade / worldbank open api
# install.packages("comtradr")

# World Bank Open Data API : wbstats
# install.packages("wbstats")
library("wbstats")
library("comtradr")

library("tidyverse")
library("dplyr")
library("janitor")


url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

####################### World Bank Indicators ###########################
### Export
wdi_trade <- wb_search(pattern = "export")
wdi_trade <- wb_search(pattern = "import")
wdi_trade <- wb_search(pattern = "gdp")
wdi_trade <- wb_search(pattern = "consumer price")
wdi_trade <- wb_search(pattern = "population")
wdi_trade <- wb_search(pattern = "investment")

# TX.VAL.MRCH.XD.WD : Export value index (2000 = 100)
# TX.QTY.MRCH.XD.WD : Export volume index (2000 = 100)
# NE.EXP.GNFS.KD    : Exports of goods and services (constant 2010 US$)
# NE.EXP.GNFS.KD.ZG : Exports of goods and services (annual % growth)
# NE.EXP.GNFS.ZS    : Exports of goods and services (% of GDP)
# TX.VAL.MANF.KD.WB :	Manufactures exports (constant US$)
# DXGSRMRCHNSKD     : Exports Merchandise, Customs, constant US$, millions
# DXGSRMRCHSAKD     : Exports Merchandise, Customs, constant US$, millions, seas. adj.
# NE.IMP.GNFS.KD    : # Imports of goods and services (constant 2015 US$)
# NE.IMP.GNFS.ZS    : # Imports of goods and services (% of GDP)
# TM.VAL.MRCH.XD.WD : # Import value index (2015 = 100)
# TM.QTY.MRCH.XD.WD : # Import volume index (2015 = 100)'
# NY.GDP.MKTP.KD    : # GDP (constant 2015 US$)
# NY.GDP.MKTP.KD.ZG : # GDP growth (annual %)
# NY.GDP.PCAP.KD    : # GDP per capita (constant 2015 US$)
# FP.CPI.TOTL       : # Consumer Price Index (2010 = 100)
# SP.POP.TOTL       : # Population, total
# SP.POP.1564.TO    : # Population ages 15-64, total
# BX.KLT.DINV.WD.GD.ZS : # Foreign direct investment, net inflows (% of GDP)
##############################################################################

my_indicators <- c("export_growth_r" = "NE.EXP.GNFS.KD.ZG"
                   ,"export_r_gdp"   = "NE.EXP.GNFS.ZS"
                   ,"export_val"     = "NE.EXP.GNFS.KD"
                   ,"export_val_idx" = "TX.VAL.MRCH.XD.WD"
                   ,"import_val"     = "NE.IMP.GNFS.KD"
                   ,"import_r_gdp"   = "NE.IMP.GNFS.ZS"
                   ,"import_val_idx" = "TM.VAL.MRCH.XD.WD"
                   ,"gdp_val"        = "NY.GDP.MKTP.KD"
                   ,"gdp_growth_r"   = "NY.GDP.MKTP.KD.ZG"
                   ,"gdp_per_cap"    = "NY.GDP.PCAP.KD"
                   ,"cpi"            = "FP.CPI.TOTL"
                   ,"pop_t"          = "SP.POP.TOTL"
                   ,"pop_1564"       = "SP.POP.1564.TO"
                   ,"fdi_r_gdp"      = "BX.KLT.DINV.WD.GD.ZS"
                   )
#### data download from World Bank through wbstats package
wdi_data <- wb_data(my_indicators, country = "all", start_date = 2005, end_date = 2025)

# create new variable: population ratio of 15-64 to total population
wdi_data <- wdi_data %>%
                  mutate(pop1564_r = round(pop_1564/pop_t, 4) * 100) 

str(wdi_data)
# merge country info and wdi data
merged_data <- country_info %>% 
            inner_join(wdi_data, by=c("iso_3"="iso3c")) %>%
            select(name, iso2c=iso_2, iso3c=iso_3, region, sub_region, date,
                   export_val, export_r_gdp, export_val_idx,
                   import_val, import_r_gdp, import_val_idx, 
                   gdp_val, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564, pop1564_r, fdi_r_gdp)

merged_data %>% filter(date >= 2020) %>% 
  # filter(fdi_r_gdp >= -10 & fdi_r_gdp <= 10 ) %>%
  drop_na(gdp_val, export_val) %>% 
  ggplot(mapping = aes(x=log(lag(gdp_val)),y=log(export_val))) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_classic()



#################### UNCOMTRADE data #######################
# set primary comtrade key
set_primary_comtrade_key("41f20f9a364a4ded8849fbf33f98b01f")

hs_codes <- ct_get_ref_table("HS")
# 2-digit: ex) "01" ->"Live animals".
hs2 <- hs_codes %>% filter(nchar(id) == 2) %>% filter(id != "99") %>% select(id, text)

# ──  parameter setting ─────────────────────────────────────────────
start_year  <- 2005
end_year    <- 2025
chunk_size  <- 5

# starts:   # ends  : 
starts <- seq(start_year, end_year, by = chunk_size)
ends   <- c(starts[-1] - 1, end_year)
cat("collecting data for the following intervals:\n")
walk2(starts, ends, ~ cat(sprintf("  %d ~ %d\n", .x, .y)))


# ct_get_data() 함수를 이용하여 특정 기간 동안의 무역 데이터를 가져올 수 있습니다.
# You can request a maximum interval of twelve years from the API
# ── API call function (에러 방지용 safe wrapper) ──────────────────
safe_ct_get <- possibly(
  .f = function(s, e) {
    cat(sprintf("\n Fetching: %d ~ %d ...", s, e))
    result <- ct_get_data(
      type                     = "goods",
      frequency                = "A",
      commodity_classification = "HS",
      commodity_code           = c("84"),
      flow_direction           = c("import"),
      reporter                 = "all_countries",
      partner                  = "World",
      start_date               = s,
      end_date                 = e,
      primary_token            = get_primary_comtrade_key()
    )
    cat(sprintf(" download %d rows\n", nrow(result)))
    return(result)
  },
  otherwise = NULL,   # 실패 시 NULL 반환
  quiet     = FALSE
)

# ── 루프 실행 & 병합 ──────────────────────────────────────────
hs2_data_list <- map2(starts, ends, safe_ct_get)

# 실패한 구간 확인
failed_idx <- which(map_lgl(hs2_data_list, is.null))
if (length(failed_idx) > 0) {
  cat("\n 추출 실패 구간:", paste(starts[failed_idx], ends[failed_idx], sep = "~"), "\n")
}

# 성공 결과만 병합
hs2_data <- hs2_data_list |>
  compact() |>          # NULL 제거
  bind_rows()

cat(sprintf("\n추출 최종 데이터: %d rows × %d cols\n", nrow(hs2_data), ncol(hs2_data)))



merged_data1 <- merged_data %>%
                  left_join(hs2_data, by=c("iso3c"="reporter_iso", "date"="ref_year")) %>% 
                      select(name, iso2c, iso3c, region, sub_region, date, 
                             export_val, export_r_gdp, export_val_idx,
                             import_val, import_r_gdp, import_val_idx, 
                             gdp_val, gdp_growth_r, gdp_per_cap, 
                             cpi, pop_t, pop_1564, pop1564_r, fdi_r_gdp, 
                             import84_val=primary_value
                             )

merged_data1 %>% filter(date >= 2020) %>% drop_na(export_val, import84_val) %>% 
  ggplot(mapping = aes(x=log(lag(import84_val)), y=log(export_val))) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_classic()


####### Pooled OLS regression with combined data ################
reg_data1 <- merged_data1 %>%
                # filter(date >= 2020) %>%
                drop_na(export_val, gdp_per_cap, gdp_val, cpi, gdp_growth_r, fdi_r_gdp, pop1564_r, import84_val)

reg_model1 <- lm(log(export_val) ~ log(lag(gdp_val))
                                   # + log(lag(gdp_per_cap))
                                   # + log(lag(cpi))
                                   # + lag(pop_t)
                                   + log(lag(import84_val))
                , data = reg_data1)

summary(reg_model1)
plot(reg_model1$residuals, pch=16, col="black", main="Residuals of OLS Regression", xlab="Index", ylab="Residuals", ylim=c(-4,4))
abline(h=0, col="red", lwd=2)



##############################################################################
######## Panel regression (within model) #####################################
# library("plm")
# reg_data1 <- merged_data1 %>%
#               # filter(date >= 2020) %>%
#               drop_na(export_val, gdp_per_cap, cpi, gdp_growth_r, fdi_r_gdp, pop_t, import84_val)
# plm_model <- plm(log(export_val) ~  log(lag(gdp_val))
#                  # + log(lag(export_val))
#                  # + log(lag(gdp_per_cap))
#                  # + log(lag(cpi))
#                  # + lag(fdi_r_gdp)
#                  # + log(lag(pop_t))
#                  + log(lag(import84_val))
#                  # + log(lag(import84_val, 2))
#                  # + log(lag(import84_val, 3))
#                  # + log(lag(import84_val, 4))
#                  , data = reg_data1, index=c("iso3c", "date"), model="within")
# 
# summary(plm_model)
# 
# resids <- as.numeric(resid(plm_model))
# fitted_vals <- as.numeric(predict(plm_model))
# # 3. scatter plot
# plot(resids,
#      xlab = "Fitted Values (y-hat)",
#      ylab = "Residuals",
#      main = "Residuals vs Fitted (Within Model)",
#      pch = 16, col = "black", ylim=c(-4,4))
# abline(h = 0, col = "red", lwd = 2) # add baseline at y=0
