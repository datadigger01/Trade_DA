# installation un_comtrade / worldbank open api

# install.packages("comtradr")
install.packages("wbstats")
library("wbstats")
library("comtradr")

library("tidyverse")
library("dplyr")
library("ggplot2")
# library("ggrepel")


url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

####################### World Bank Indicators ###########################
### Export


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
                   ,"import_val"   = "NE.IMP.GNFS.KD"
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

# create new variable: population ratio of 15-64 to total population
merged_data <- merged_data %>%
                mutate(pop1564_r = round(pop_1564/pop_t, 4) * 100)

unique(merged_data$sub_region)
merged_data %>% filter(date >= 2020) %>% 
                # filter(fdi_r_gdp >= -10 & fdi_r_gdp <= 10 ) %>%
                drop_na(gdp_growth_r, export_r_gdp) %>% 
                ggplot(mapping = aes(x=log(lag(gdp_per_cap,1)),y=log(export_val))) +
                geom_point() +
                # geom_smooth(method = "lm", se=F) +
                theme_classic()


## OLS regression
reg_data <- merged_data %>% filter(date >= 2010) %>% drop_na(export_val, gdp_per_cap, cpi, fdi_r_gdp, pop_t)
reg_model <- lm(log(export_val) ~ log(lag(gdp_per_cap)) 
                                      #  + log(lag(gdp_val))
                                      # + lag(cpi)
                                      # + lag(fdi_r_gdp)
                                      # + log(lag(pop_t))
                                      , 
                                      data = reg_data)
summary(reg_model)
# plot(reg_model,1)

plot(reg_model$fitted.values, reg_model$residuals, pch=16, main="Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals", ylim=c(-5,5))
abline(h=0, col="red", lwd=2)