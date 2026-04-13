#########################################################
library("tidyverse")
library("wbstats")
library("comtradr")


url <- "https://raw.githubusercontent.com/datadigger01/Trade_DA/main/Data/2026D/country_region.csv"
country_info <- read_csv(url)


##### IMF API with R ####################################
install.packages("rsdmx")
library(rsdmx)

# agency and id identify the dataset you are interested in
flowref <- 'IMF.STA,IMTS' # STA: International Financial Statistics (IFS), ITG: International Trade in Goods and Services
                          # IMTS: International Merchandise Trade Statistics (IMTS)

# filter identifies the subset of the dataset you want.
filter <- '.XG_FOB_USD..A' # XG: export of goods, FOB value, USD; A: annual data


# readSDMX() retrieves the data from the API and returns it as an SDMX object.
imf_data <- as.data.frame(readSDMX(providerId = 'IMF_DATA',
                                   resource = 'data',
                                   flowRef = flowref,
                                   key = filter,
                                   start = 2000, # set the time period: yearly -> yyyy, quarterly-> yyyy-Qn, monthly->yyyy-mm 
                                   end = 2015))
str(imf_data)
names(imf_data) <- tolower(names(imf_data)) # convert column names to lower case for easier handling

str(imf_data)

imf_data <- imf_data %>% 
                mutate( year = as.numeric(time_period),
                        export_goods_val = as.numeric(obs_value)) %>% 
                select(country, partner=counterpart_country, indicator, frequency, year, export_goods_val)


### World Bank Open Data API : wbstats
my_indicators <- c("gdp_val"        = "NY.GDP.MKTP.KD"
                   ,"gdp_growth_r"   = "NY.GDP.MKTP.KD.ZG"
                   ,"gdp_per_cap"    = "NY.GDP.PCAP.KD"
                   ,"cpi"            = "FP.CPI.TOTL"
                   ,"pop_t"          = "SP.POP.TOTL"
                   ,"pop_1564"       = "SP.POP.1564.TO"
                   ,"fdi_r_gdp"      = "BX.KLT.DINV.WD.GD.ZS"
                   ,"fdi_inflow"     = "BX.KLT.DINV.CD.WD"
)
wdi_data <- wb_data(my_indicators, country = "all", start_date = 2000, end_date = 2015)

# filter ASEAN countries in the country_info dataset
ASEAN <- c("Brunei", "Cambodia", "Indonesia", "Lao", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Viet")
target_countries <- country_info %>%
  filter(grepl(paste(ASEAN, collapse = "|"), name, ignore.case = TRUE))

# merge the IMF export data with the target ASEAN countries data
kor_export_asean <- imf_data %>%
                      filter(country == "KOR") %>% 
                      inner_join(target_countries, by = c("partner" = "iso_3")) %>% 
                      select(country, partner, name, year, export_goods_val)

# merge the export data with the World Bank indicators data
kor_export_asean <- kor_export_asean %>% 
                     inner_join(wdi_data, by = c("partner" = "iso3c", "year" = "date")) %>% 
                     select(country=country.x, partner, name, year, export_goods_val, gdp_val, gdp_growth_r, gdp_per_cap, cpi, pop_t, pop_1564, fdi_r_gdp, fdi_inflow)
                     
# FTA status with ASEAN countries by year
fta_year <- c(
    "SGP"   = 2006,  # Singapore
    "MYS"   = 2007,  # Malaysia 
    "THA"   = 2010,  # Thailand: delayed due to political instability, originally planned for 2007
    "IDN"   = 2007,  # Indonesia
    "PHL"   = 2007,  # Philippines
    "BRN"   = 2007,  # Brunei
    "VNM"   = 2007,  # Vietnam
    "KHM"   = 2007,  # Cambodia
    "LAO"   = 2007,  # Lao PDR
    "MMR"   = 2007   # Myanmar
    )
kor_export_asean <- kor_export_asean %>%
      mutate(
            fta_year   = fta_year[partner],       # mapping partner country to its FTA year
            fta_status = case_when(
                          is.na(year)          ~ NA_character_,
                          year >= fta_year     ~ "FTA",
                          year < fta_year      ~ "Non-FTA",
                          TRUE                 ~ NA_character_
                          )
            )

## visualize the relationship between export value and GDP value for ASEAN countries
kor_export_asean %>% 
    # filter(year <= 2015) %>% 
    drop_na(gdp_val, export_goods_val) %>% 
    ggplot(mapping = aes(x=log(lag(gdp_val)),y=log(export_goods_val))) +
    geom_point() +
    # geom_smooth(method = "lm", se=F) +
    theme_classic()


## OLS regression with lagged independent variables to check the relationship between export value and GDP value, controlling for other factors
reg_data <- kor_export_asean %>% 
                          # filter(year < 2015) %>% 
                          drop_na(export_goods_val, gdp_val, cpi, fdi_inflow, pop_t)

#reg_data <- reg_data %>%mutate(fta_status = relevel(as.factor(fta_status), ref = "Non-FTA"))
reg_model <- lm(log(export_goods_val) ~ as.factor(fta_status)
                                        + log(lag(gdp_val,1))
                                        + log(lag(cpi,1))
                                        + log(lag(pop_t,1))
                                        , data = reg_data)
summary(reg_model)
