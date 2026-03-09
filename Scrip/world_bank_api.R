install.packages("WDI")
library(WDI)

# WDIsearch() 함수는 WDI 데이터베이스에서 특정 키워드와 일치하는 지표를 검색하는 데 사용됩니다.
# string: 검색할 키워드입니다. 예를 들어, "export" 또는 "import"와 같은 단어를 입력할 수 있습니다.
# field: 검색할 필드를 지정합니다. 일반적으로 "name"을 사용

# import
wdi_trade <- WDIsearch(string = "export", field = "name", short = FALSE ,cache = NULL)
wdi_export <- WDI(country = "all", indicator = 'TX.VAL.MRCH.XD.WD', start = 2010, end = 2025)  #	TX.VAL.MRCH.XD.WD:Export value index (2015 = 100)
# export
wdi_trade <- WDIsearch(string = "import", field = "name", short = FALSE ,cache = NULL)
wdi_import <- WDI(country = "all", indicator = 'TM.VAL.MRCH.XD.WD', start = 2010, end = 2025)  #	TM.VAL.MRCH.XD.WD:Import value index (2015 = 100)
# FDI
wdi_fdi <- WDIsearch(string = "investment", field = "name", short = FALSE ,cache = NULL)
wdi_fdi_out <- WDI(country = "all", indicator = 'BM.KLT.DINV.WD.GD.ZS', start = 2010, end = 2025)  # BM.KLT.DINV.WD.GD.ZS:Foreign direct investment, net outflows (% of GDP)
# wdi_fdi_in <- WDI(country = "all", indicator = 'BN.KLT.DINV.CD.ZS', start = 2010, end = 2025)

# how to merge country_info with wdi_export dataset by iso2c code
# Assuming you have a country_info dataset with a column named 'iso2c' and wdi_export dataset with a column named 'iso2c'
# You can use the merge function in R to merge the two datasets based on the 'iso2c' column
merged_data <- merge(country_info, wdi_export, by.x = "iso_2", by.y="iso2c", all.x = TRUE) %>% 
  select(iso_2, country.x, year, TX.VAL.MRCH.XD.WD) %>% 
  rename(country=country.x, export_value_index = TX.VAL.MRCH.XD.WD)

merged_data <- merge(merged_data, wdi_import, by.x = c("iso_2","year"), by.y= c("iso2c","year"), all.x = TRUE) %>% 
  select(iso_2, country.x, year, export_value_index, TM.VAL.MRCH.XD.WD) %>% 
  rename(country=country.x, import_value_index = TM.VAL.MRCH.XD.WD)

merged_data <- merge(merged_data, wdi_fdi_out, by.x = c("iso_2","year"), by.y= c("iso2c","year"), all.x = TRUE) %>% 
  select(iso_2, country.x, year, export_value_index, import_value_index, BM.KLT.DINV.WD.GD.ZS) %>% 
  rename(country=country.x, fdi_outflow_gdp = BM.KLT.DINV.WD.GD.ZS) 

str(merged_data)




# create line graph of export_value_index by country with merged_data
library(ggplot2)

# select several countries in the merged_data and draw the line graph with ggplot2
countries_to_plot <- c('KR','JP','CN') # ISO2C codes for South Korea, Japan
target_data <- merged_data %>% filter(iso_2 %in% countries_to_plot)


ggplot(target_data, aes(x = year, y = export_value_index, color =country)) +
  geom_line() +
  labs(title = "Export Value Index by Country", x = "Year", y = "Export Value Index") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) # Adjust legend to have multiple rows
