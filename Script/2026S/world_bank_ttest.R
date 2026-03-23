#install.packages("WDI")
library(WDI)
library(tidyverse)
library(ggplot2)

# WDIsearch() 함수는 WDI 데이터베이스에서 특정 키워드와 일치하는 지표를 검색하는 데 사용됩니다.
# string: 검색할 키워드입니다. 예를 들어, "export" 또는 "import"와 같은 단어를 입력할 수 있습니다.
# field: 검색할 필드를 지정합니다. 일반적으로 "name"을 사용

# export
wdi_trade <- WDIsearch(string = "export", field = "name", short = FALSE ,cache = NULL)
wdi_export <- WDI(country = "all", indicator = 'NE.EXP.GNFS.ZS', start = 2010, end = 2025)  #	NE.EXP.GNFS.ZS: Exports of goods and services (% of GDP)
#write_csv(wdi_export, "D:/wdi_export.csv")

# import
wdi_trade <- WDIsearch(string = "import", field = "name", short = FALSE ,cache = NULL)
wdi_import <- WDI(country = "all", indicator = 'NE.IMP.GNFS.ZS', start = 2010, end = 2025)  #	NE.IMP.GNFS.ZS:Imports of goods and services (% of GDP)

# GDP growth
wdi_gdp <- WDIsearch(string = "gdp growth", field = "name", short = FALSE ,cache = NULL)
wdi_gdpgrowth <- WDI(country = "all", indicator = 'NY.GDP.MKTP.KD.ZG', start = 2010, end = 2025)  # NV.AGR.TOTL.ZG:GDP growth (annual %)

url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)


# how to merge country_info with wdi_export dataset by iso2c code
# merged_data <- merge(country_info, wdi_export, by.x = "iso_2", by.y="iso2c", all.x = TRUE) %>% 
#   select(iso_2, country.x, year, TX.VAL.MRCH.XD.WD) %>% 
#   rename(country=country.x, export_value_index = TX.VAL.MRCH.XD.WD)
str(country_info)


merged_data <- country_info %>%
  left_join(wdi_export, by = c("iso_2" = "iso2c")) %>%
  select(iso_2, iso_3, name, region, sub_region, year, export_ratio = NE.EXP.GNFS.ZS)

str(merged_data)
# merged_data <- merge(merged_data, wdi_import, by.x = c("iso_2","year"), by.y= c("iso2c","year"), all.x = TRUE) %>% 
#   select(iso_2, country.x, year, export_value_index, TM.VAL.MRCH.XD.WD) %>% 
#   rename(country=country.x, import_value_index = TM.VAL.MRCH.XD.WD)
merged_data <- merged_data %>%
  left_join(wdi_import, by = c("iso_2" = "iso2c", "year" = "year")) %>%
  select(iso_2, iso_3, name, region, sub_region, year, export_ratio, import_ratio = NE.IMP.GNFS.ZS)

merged_data <- merged_data %>%
  left_join(wdi_gdpgrowth, by = c("iso_2" = "iso2c", "year" = "year")) %>%
  select(iso_2, iso_3, name, region, sub_region, year, export_ratio, import_ratio, gdp_growth = NY.GDP.MKTP.KD.ZG)


unique(merged_data$region)
merged_data %>% filter(year==2020, region %in% c('Asia','Americas')) %>% 
                group_by(region) %>%  
                summarise(avg_gdpgrowth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")
# distribution by region or sub_region  
merged_data %>% filter(year==2020, region %in% c('Asia','Americas')) %>% 
            select(iso_3, name, region, sub_region,year, gdp_growth) %>% drop_na(gdp_growth) %>% 
            ggplot(data=., aes(gdp_growth, fill = region)) +
            geom_density(alpha=0.5) +
            # geom_vline(xintercept =-4.55, color='red') +
            # geom_vline(xintercept =-9.55, color='blue') +
            theme_bw()


###################################################################################
### Independent sample t-test for GDP growth rate between Asia and Americas in 2020
###################################################################################
t_test_data <- merged_data %>%
  filter(region %in% c("Asia","Americas"), year==2020) %>%
  select(name, iso_3, region, year, gdp_growth) %>% drop_na(gdp_growth)

# perform independent sample t-test
t.test(gdp_growth ~ region, data = t_test_data)

# t_test_result <- t.test(gdp_growth ~ region, data = t_test_data)
# t_test_result$statistic

##############################################################################################
## Paired sample t-test for GDP growth rate between Asia and Americas before COVID/after COVID
##############################################################################################
unique(merged_data$region)
merged_data %>% filter(year %in% c(2019,2024), region %in% c('Asia')) %>% 
  # filter(!iso_3 %in% c('PSE','TLS')) %>%
  select(name, iso_3, region, sub_region, year, gdp_growth) %>% drop_na(gdp_growth) %>% 
  ggplot(data=., aes(gdp_growth, fill = as.factor(year))) +
  geom_density(alpha=0.7) +
  # geom_vline(xintercept =1.77, color='red') +
  # geom_vline(xintercept =2.58, color='blue') +
  theme_bw()

# # calculate mean of gdp_growth rate
# merged_data %>% filter(year %in% c(2019, 2024), region %in% c("Americas"))  %>% 
#   filter(!iso_3 %in% c('GUY','VEN')) %>% 
#   select(name, iso_3, region, sub_region, year, gdp_growth) %>% drop_na(gdp_growth) %>%
#   group_by(year) %>%
#   summarise(avg_gdpgrowth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")


# paired t-test for GDP growth between 2019 and 2024
paired_t_test_data <- merged_data %>%
  filter(region %in% c("Asia"), year %in% c(2019, 2024)) %>% 
  # filter(!iso_3 %in% c('PSE','VEN')) %>%
  select(name, iso_3, region, sub_region, year, gdp_growth) %>% 
  pivot_wider(id_cols = c(name,iso_3,region), names_from = year, values_from = gdp_growth, names_prefix = "gdp_growth_") %>% 
  drop_na(gdp_growth_2019, gdp_growth_2024)
# perform paired t-test
t.test(paired_t_test_data$gdp_growth_2024, paired_t_test_data$gdp_growth_2019, paired = TRUE)


# Another way of performing paired t-test
merged_data %>% 
  filter(region == "Asia", year %in% c(2019, 2024)) %>%
  # filter(!iso_3 %in% c('PSE','VEN')) %>%
  select(name, iso_3, region, sub_region, year, gdp_growth) %>% drop_na(gdp_growth) %>%
  pivot_wider(id_cols = c(name,iso_3,region), names_from = year, values_from = gdp_growth, names_prefix = "gdp_growth_") %>%
  drop_na(gdp_growth_2019, gdp_growth_2024) %>%
  with(t.test(gdp_growth_2024,gdp_growth_2019, paired = TRUE, data = .))
