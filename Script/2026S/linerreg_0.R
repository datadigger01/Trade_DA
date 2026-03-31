# installation un_comtrade / worldbank open api

# install.packages("comtradr")
# install.packages("WDI")
library("WDI")
library("comtradr")

library("tidyverse")
library("dplyr")
library("ggplot2")
# library("ggrepel")


url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)

####################### World Bank Indicators ###########################
### Export
wdi_trade <- WDIsearch(string = "export", field = "name", short = FALSE ,cache = NULL)
wdi_export1 <- WDI(country = "all", indicator = 'NE.EXP.GNFS.KD', start = 2010, end = 2025)  #	NE.EXP.GNFS.KD: Export of good and service(constant 2015 USD) 
wdi_export2 <- WDI(country = "all", indicator = 'NE.EXP.GNFS.ZS', start = 2010, end = 2025)  #	NE.EXP.GNFS.ZD: Export of good and service(% of GDP) 
#wdi_export3 <- WDI(country = "all", indicator = 'TX.VAL.MRCH.XD.WD', start = 2010, end = 2025) # Export volume index (2015 = 100)

### Import
wdi_trade <- WDIsearch(string = "import", field = "name", short = FALSE ,cache = NULL)
wdi_import1 <- WDI(country = "all", indicator = 'NE.IMP.GNFS.KD', start = 2010, end = 2025)  #	NE.IMP.GNFS.ZS:Imports of goods and services(constant 2015 USD)
wdi_import2 <- WDI(country = "all", indicator = 'NE.IMP.GNFS.ZS', start = 2010, end = 2025)  #	NE.EXP.GNFS.ZD: Export of good and service(% of GDP) 
#wdi_import3 <- WDI(country = "all", indicator = 'TM.VAL.MRCH.XD.WD', start = 2010, end = 2025) 

### GDP
wdi_trade <- WDIsearch(string = "gdp", field = "name", short = FALSE ,cache = NULL)
wdi_gdpgrowth <- WDI(country = "all", indicator = 'NY.GDP.MKTP.KD.ZG', start = 2010, end = 2025)  # NV.AGR.TOTL.ZG:GDP growth (annual %)
wdi_gdp_per <- WDI(country = "all", indicator = 'NY.GDP.PCAP.KD', start = 2010, end = 2025)  # NY.GDP.PCAP.KD: GDP per Capita(constant 2015 USD)

### Inflation
wdi_trade <- WDIsearch(string = "consumer price", field = "name", short = FALSE ,cache = NULL)
wdi_cpi <- WDI(country = "all", indicator = 'FP.CPI.TOTL', start = 2010, end = 2025)  # FP.CPI.TOTL: consumer price index(2010=100)

### Population
wdi_trade <- WDIsearch(string = "population", field = "name", short = FALSE ,cache = NULL)
wdi_pop_t <- WDI(country = "all", indicator = 'SP.POP.TOTL', start = 2010, end = 2025) # SP.POP.TOTL : Population, total
wdi_pop_1564 <- WDI(country = "all", indicator = 'SP.POP.1564.TO', start = 2010, end = 2025) # SP.POP.1564.TO : Population ages 15-64, total

### FDI(Foreign Direct Investment) 
wdi_trade <- WDIsearch(string = "investment", field = "name", short = FALSE ,cache = NULL)
wdi_fdi_r <- WDI(country = "all", indicator = 'BX.KLT.DINV.WD.GD.ZS', start = 2010, end = 2025) # BX.KLT.DINV.WD.GD.ZS : Foreign direct investment, net inflows (% of GDP)
# wdi_fdi_out <- WDI(country = "all", indicator = 'BM.KLT.DINV.CD.WD', start = 2010, end = 2025) # BX.KLT.DINV.WD.GD.ZS : Foreign direct investment, net inflows (% of GDP)
# wdi_fdi_out <- WDI(country = "all", indicator = 'BN.KLT.DINV.CD.DRS', start = 2010, end = 2025) # BX.KLT.DINV.WD.GD.ZS : Foreign direct investment, net inflows (% of GDP)


colnames(merged_data)
merged_data <- country_info %>% 
            inner_join(wdi_export1, by=c("iso_3"="iso3c")) %>% select(name, iso_2, iso_3, region, sub_region, year, export_value=NE.EXP.GNFS.KD) %>% 
            left_join(wdi_export2, by=c("iso_3"="iso3c", "year"="year")) %>% 
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp=NE.EXP.GNFS.ZS) %>%
            left_join(wdi_import1, by=c("iso_3"="iso3c", "year"="year")) %>%
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value=NE.IMP.GNFS.KD) %>% 
            left_join(wdi_import2, by=c("iso_3"="iso3c", "year"="year")) %>%
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp=NE.IMP.GNFS.ZS) %>%
            left_join(wdi_gdpgrowth, by=c("iso_3"="iso3c", "year"="year")) %>% 
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp, gdp_growth_r=NY.GDP.MKTP.KD.ZG) %>%
            left_join(wdi_gdp_per, by=c("iso_3"="iso3c", "year"="year")) %>% 
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp, gdp_growth_r, gdp_per_cap=NY.GDP.PCAP.KD) %>% 
            left_join(wdi_cpi, by=c("iso_3"="iso3c", "year"="year")) %>%
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp, gdp_growth_r, gdp_per_cap, cpi=FP.CPI.TOTL) %>% 
            left_join(wdi_pop_t, by=c("iso_3"="iso3c", "year"="year")) %>%
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp, gdp_growth_r, gdp_per_cap, cpi, pop_t=SP.POP.TOTL) %>% 
            left_join(wdi_pop_1564, by=c("iso_3"="iso3c", "year"="year")) %>%
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564=SP.POP.1564.TO) %>% 
            left_join(wdi_fdi_r, by=c("iso_3"="iso3c", "year"="year")) %>%
                  select(name, iso_2, iso_3, region, sub_region, year, export_value, export_r_gdp, import_value, import_r_gdp, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564, fdi_r_gdp=BX.KLT.DINV.WD.GD.ZS)

# create new variable: population ratio of 15-64 to total population
merged_data <- merged_data %>%
                mutate(pop1564_r = round(pop_1564/pop_t, 4) * 100)

  unique(merged_data$sub_region)
merged_data %>% filter(year >= 2020) %>% 
                # filter(fdi_r_gdp >= -10 & fdi_r_gdp <= 10 ) %>%
                drop_na(gdp_growth_r, export_r_gdp) %>% 
          ggplot(mapping = aes(y=log(export_value),x=log(lag(gdp_per_cap)))) +
          geom_point() +
          geom_smooth(method = "lm", se=F) +
          theme_classic()


## OLS regression
reg_data <- merged_data %>% filter(year >= 2010) %>% drop_na(export_value, gdp_per_cap, cpi)
reg_model <- lm(log(export_value) ~  log(lag(gdp_per_cap)) 
                                      # + log(lag(export_value,2))
                                      # + lag(cpi)
                                      # + lag(fdi_r_gdp)
                                      # + log(lag(pop_t))
                                      , 
                                      data = reg_data)
summary(reg_model)
# plot(reg_model,1)

plot(reg_model$fitted.values, reg_model$residuals, pch=16, main="Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals", ylim=c(-5,5))
abline(h=0, col="red", lwd=2)
