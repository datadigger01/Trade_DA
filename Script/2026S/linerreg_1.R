# installation un_comtrade / worldbank open api
# install.packages("comtradr")

# World Bank Open Data API : wbstats
install.packages("wbstats")
library("wbstats")
library("comtradr")

library("tidyverse")
library("dplyr")
library("janitor")


url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

####################### World Bank Indicators ###########################
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
# BX.KLT.DINV.CD.WD : # Foreign direct investment, net inflows (BoP, current US$)


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
                   ,"fdi_inflow"     = "BX.KLT.DINV.CD.WD"
                  )
wdi_data <- wb_data(my_indicators, country = "all", start_date = 2005, end_date = 2025)

wdi_data <- wdi_data %>%
    mutate(pop1564_r = round(pop_1564/pop_t, 4) * 100) # create new variable: population ratio of 15-64 to total population

str(wdi_data)
merged_data <- country_info %>% 
  inner_join(wdi_data, by=c("iso_3"="iso3c")) %>%
  select(name, iso2c=iso_2, iso3c=iso_3, region, sub_region, date,
         export_val, export_r_gdp, export_val_idx,
         import_val, import_r_gdp, import_val_idx, 
         gdp_val, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564, pop1564_r, 
         fdi_r_gdp, fdi_inflow
         )

## Covid-19 pandemic shock and trade: scatter plot of export value and GDP value
merged_data <- merged_data %>% 
                  mutate(covid_period = ifelse(date %in% c(2020,2021,2022), "Y", "N"))


merged_data1 %>% 
  filter(date >= 2020) %>%
  drop_na(export_val, gdp_val, gdp_lag) %>%
  ggplot(mapping = aes(x=log(lag(gdp_val,1)),y=log(export_val))) +
  # ggplot(mapping = aes(x=log(lag(gdp_val,1)),y=log(export_val),color=covid_period)) +
  geom_point() +
  # geom_smooth(method = "lm", se=F) +
  theme_classic()


####### Pooled OLS regression with combined data ################
reg_data <- merged_data %>%
              filter(date >= 2015) %>%
              drop_na(export_val, gdp_per_cap, cpi, gdp_growth_r, fdi_inflow, pop_t)

reg_model <- lm(log(export_val) ~ log(lag(gdp_val,1))
                # + log(lag(gdp_per_cap,1))
                # + log(lag(cpi,1))
                # + log(lag(pop_t,1))
                # + as.factor(covid_period)
                , data = reg_data)

summary(reg_model)

plot(reg_model$residuals, pch=16, col="black", main="Residuals of OLS Regression", xlab="Index", ylab="Residuals", ylim=c(-4,4))
abline(h=0, col="red", lwd=2)



#### Panel regression (within model) ##########
###############################################
install.packages("plm")
library("plm")
reg_data <- merged_data %>%
              filter(date >= 2015) %>%
              drop_na(export_val, gdp_per_cap, cpi, pop1564_r, gdp_val)
plm_model <- plm(log(export_val) ~ log(lag(gdp_val,1))
                                    # + log(lag(export_val,1))
                                    # + log(lag(gdp_per_cap))
                                    # + log(lag(cpi))
                                    # + lag(pop1564_r,1)
                                    # + as.factor(covid_period)
                 , data = reg_data, index=c("iso3c", "date"), model="within")

summary(plm_model)

resids <- as.numeric(resid(plm_model))
fitted_vals <- as.numeric(predict(plm_model))
# 3. scatter plot
plot(resids,
     xlab = "Fitted Values (y-hat)", 
     ylab = "Residuals",
     main = "Residuals vs Fitted (Within Model)",
     pch = 16, col = "black", ylim=c(-4,4))
abline(h = 0, col = "red", lwd = 2) # add baseline at y=0
