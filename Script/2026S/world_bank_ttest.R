#install.packages("WDI")
library(WDI)
library(tidyverse)


# WDIsearch() 함수는 WDI 데이터베이스에서 특정 키워드와 일치하는 지표를 검색하는 데 사용됩니다.
# string: 검색할 키워드입니다. 예를 들어, "export" 또는 "import"와 같은 단어를 입력할 수 있습니다.
# field: 검색할 필드를 지정합니다. 일반적으로 "name"을 사용

# export
wdi_trade <- WDIsearch(string = "export", field = "name", short = FALSE ,cache = NULL)
wdi_export <- WDI(country = "all", indicator = 'TX.VAL.MRCH.XD.WD', start = 2010, end = 2025)  #	TX.VAL.MRCH.XD.WD:Export value index (2015 = 100)
#write_csv(wdi_export, "D:/wdi_export.csv")

# import
wdi_trade <- WDIsearch(string = "import", field = "name", short = FALSE ,cache = NULL)
wdi_import <- WDI(country = "all", indicator = 'TM.VAL.MRCH.XD.WD', start = 2010, end = 2025)  #	TM.VAL.MRCH.XD.WD:Import value index (2015 = 100)
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
  select(iso_2, iso_3, name, region, sub_region, year, export_value_index = TX.VAL.MRCH.XD.WD)

str(merged_data)
# merged_data <- merge(merged_data, wdi_import, by.x = c("iso_2","year"), by.y= c("iso2c","year"), all.x = TRUE) %>% 
#   select(iso_2, country.x, year, export_value_index, TM.VAL.MRCH.XD.WD) %>% 
#   rename(country=country.x, import_value_index = TM.VAL.MRCH.XD.WD)
merged_data <- merged_data %>%
  left_join(wdi_import, by = c("iso_2" = "iso2c", "year" = "year")) %>%
  select(iso_2, iso_3, name, region, sub_region, year, export_value_index, import_value_index = TM.VAL.MRCH.XD.WD)

merged_data <- merged_data %>%
  left_join(wdi_gdpgrowth, by = c("iso_2" = "iso2c", "year" = "year")) %>%
  select(iso_2, iso_3, name, region, sub_region, year, export_value_index, import_value_index, gdp_growth = NY.GDP.MKTP.KD.ZG)


str(merged_data)


# create line graph of export_value_index by country with merged_data
library(ggplot2)

#### select several countries in the merged_data and draw the line graph with ggplot2

unique(merged_data$sub_region)
target_countries <- c("Eastern Asia")
target_data <- merged_data %>% filter(sub_region %in% target_countries) %>% 
  filter(!name %in% c("Korea, Democratic People's Republic of","Macao","Mongolia"))
unique(target_data$name)

# unique(merged_data$sub_region)
# target_countries <- c("South-eastern Asia")
# target_data <- merged_data %>% filter(sub_region %in% target_countries) %>% 
#                                 filter(!name %in% c('Brunei Darussalam','Timor-Leste'))
# unique(target_data$name)

unique(merged_data$sub_region)
target_countries <- c("Western Europe")
target_data <- merged_data %>% filter(sub_region %in% target_countries) %>%
  filter(!name %in% c('Liechtenstein'))
unique(target_data$name)


###### line graph of export_value_index by country with target_data
ggplot(target_data, aes(x = year, y = export_value_index, color = name)) +
  geom_line(linewidth = 1.2) +
  # scale_color_manual(values = c("Korea, Republic of" = "black", "China" = "red", "Japan" = "blue")) +
  labs(title = "Export by Country", x = "Year", y = "Export Value Index") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(target_data, aes(x = year, y = gdp_growth, color = name)) +
  geom_line(linewidth = 1.2) +
  # scale_color_manual(values = c("Korea, Republic of" = "black", "China" = "red", "Japan" = "blue")) +
  labs(title = "GDP growth by Country", x = "Year", y = "GDP growth") +
  theme_minimal() +
  theme(legend.position = "bottom")


##### Boxplot of export_value_index by sub_region with merged_data
# write_csv(merged_data, "D:/merged_data.csv")
unique(merged_data$sub_region)
target_data <- merged_data %>%
  filter(sub_region == "South-eastern Asia", year %in% c(2019, 2024)) %>%
  mutate(year = as.factor(year))

# GDP Growth Boxplot
ggplot(target_data, aes(x = year, y = gdp_growth, fill = year)) +
  geom_boxplot(linewidth = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(values = c("2019" = "#457B9D", "2024" = "#E63946")) +
  labs(
    title = "GDP Growth(2019 vs 2024)",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



### t-test for GDP growth between 2019 and 2024 in South-eastern Asia
# filter data for South-eastern Asia and years 2019 and 2024
t_test_data <- merged_data %>%
                filter(sub_region %in% c("South-eastern Asia","Latin America and the Caribbean"), year==2020) %>%
                select(name, iso_3, sub_region, year, gdp_growth) %>% drop_na(gdp_growth)

# perform t-test
t.test(gdp_growth ~ sub_region, data = t_test_data)
# t_test_result <- t.test(gdp_growth ~ sub_region, data = t_test_data)
# t_test_result$statistic

# another way of performing t-test
merged_data %>%
  filter(sub_region %in% c("South-eastern Asia","Latin America and the Caribbean"), year==2020) %>%
  select(name, iso_3, sub_region, year, gdp_growth) %>% drop_na(gdp_growth) %>% 
  t.test(gdp_growth ~ sub_region, data = .)


# paired t-test for GDP growth between 2019 and 2024 in South-eastern Asia
# filter data for South-eastern Asia and years 2019 and 2024
paired_t_test_data <- merged_data %>%
  filter(sub_region == "South-eastern Asia", year %in% c(2019, 2024)) %>%
  select(name, iso_3, sub_region, year, gdp_growth) %>% drop_na(gdp_growth) %>%
  pivot_wider(names_from = year, values_from = gdp_growth, names_prefix = "gdp_growth_")
# perform paired t-test
t.test(paired_t_test_data$gdp_growth_2019, paired_t_test_data$gdp_growth_2024, paired = TRUE)

# another way of performing paired t-test
merged_data %>% 
  filter(sub_region == "South-eastern Asia", year %in% c(2019, 2024)) %>%
  select(name, iso_3, sub_region, year, gdp_growth) %>% drop_na(gdp_growth) %>%
  pivot_wider(names_from = year, values_from = gdp_growth, names_prefix = "gdp_growth_") %>%
  with(t.test(gdp_growth_2019,gdp_growth_2024, paired = TRUE, data = .))
