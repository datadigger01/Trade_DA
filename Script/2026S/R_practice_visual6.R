# From CRAN
install.packages("esquisse")
library("esquisse")

library(tidyverse)
# library(ggplot2)



df <- read.csv("./WtoData_20240327050855.csv")

##### column selection and renaming
df1 <- df %>% select(Indicator.Code, 
                      Reporting.Economy.ISO3A.Code,
                      Reporting.Economy,
                      Product.Sector.Code,
                      Product.Sector,
                      Unit.Code,
                      year = Year,
                      value = Value) %>% 
              rename(Ind_cd =Indicator.Code,
                      iso3c  = Reporting.Economy.ISO3A.Code,
                      country = Reporting.Economy,
                      prod_cd = Product.Sector.Code,
                      prod_ds = Product.Sector,
                      unit = Unit.Code) %>% 
              mutate( EXP_IMP = case_when( Ind_cd == "ITS_MTP_AMF" ~ "import",
                                           Ind_cd == "ITS_MTP_AXF" ~ "export"
                                          )
                    )

#### pivot_wider()
df2 <- df1 %>% select(iso3c, country, year, prod_cd, EXP_IMP, value) %>%
                pivot_wider(names_from = c(prod_cd, EXP_IMP), values_from = value) %>%
                arrange(iso3c, year)


#### add region info to df2
region_info <- read.csv("./country_region_code.csv")
df3 <- region_info %>% select(alpha.3, region, sub.region, intermediate.region) %>% 
  rename(iso3c = alpha.3, 
         sub_region = sub.region, 
         inter_region=intermediate.region) %>% 
  inner_join(., df2, by="iso3c") %>%
  relocate(country, iso3c, region, sub_region, inter_region, year, TO_export, TO_import)

#### load the gdp growth rate and inflation rate(cusumer price index) file from world bank data
gdp_grw <- read_csv("./API_NY.GDP.MKTP.KD.ZG_DS2.csv", col_names = T, skip = 4)
inf_cpi <- read_csv("./API_FP.CPI.TOTL.ZG_DS2.csv", col_names = T, skip = 4)

gdp_grw %>% select("Country Code", "2000":"2023") %>%
  pivot_longer( cols = "2000":"2023", names_to = "year", values_to = "GDP_growth_rate") %>%
  mutate(year = as.numeric(year)) %>% 
  rename(iso3c="Country Code") -> gdp_grw_f

inf_cpi %>% select("Country Code", "2000":"2023") %>% 
  pivot_longer( cols = "2000":"2023", names_to = "year", values_to = "Inflation_rate") %>% 
  mutate(year = as.numeric(year)) %>% 
  rename(iso3c="Country Code") -> info_cpi_f


#### merge these two files with export & import df
left_join(df3, gdp_grw_f, by=c("iso3c"="iso3c","year"="year")) %>%
  left_join(., info_cpi_f, by=c("iso3c"="iso3c", "year"="year")) -> final_df

# derive new variables using lag function
lm_df_lag <- final_df %>% arrange(country, year) %>% 
                          mutate(TO_export_pre = lag(TO_export),
                                  GDP_gr_rate_pre = lag(GDP_growth_rate),  # the very previous gdp growth rate
                                  Inflation_rate_pre = lag(Inflation_rate), # the very previous inflation rate
                                  country_pre = lag(country)) %>%          # flag that the previous value has the same country name 
                          relocate(TO_export_pre, .after = TO_export) %>% 
                          relocate(GDP_gr_rate_pre, .after = GDP_growth_rate) %>%
                          relocate(country_pre, .after = country)


## source code from "esquisse" package
# example 1
final_df %>%
  filter(!(sub_region %in% c("Polynesia", "Melanesia", "Micronesia"))) %>%
  filter(!(inter_region %in% 
             "Caribbean")) %>%
  ggplot() +
  aes(x = year, y = TO_export, group = iso3c) +
  geom_line(colour = "#112446") +
  theme_bw() +
  facet_wrap(vars(region), scales = "free_y")

# example 2
lm_df_lag %>%
  filter(region %in% "Asia") %>%
  filter(sub_region %in% c("Eastern Asia")) %>%
  # mutate(Is_KOR = iso3c=='KOR') %>%
  ggplot() +
  aes(x = year, y = TO_export, group = iso3c) +
  # aes(x = year, y = TO_export, color=Is_KOR, group = iso3c) +
  geom_line() +
  scale_y_continuous(trans = "log") +
  # scale_color_manual(breaks = c(FALSE,TRUE),
  #                    values = c('lightgrey','dodgerblue')) +
  # scale_size_manual(breaks = c(FALSE,TRUE),
  #                   values = c(0.3,2)) +
  labs(y = "log(Export Index)", title = "Export in Asia") +
  theme_minimal()


# example 3
lm_df_lag %>%
 filter(region %in% "Asia") %>%
 filter(!(sub_region %in% c("Polynesia", "Melanesia", "Micronesia"))) %>%
 filter(year >= 2020L & year <= 2020L) %>%
 filter(GDP_growth_rate >= -15L & GDP_growth_rate <= 15L | is.na(GDP_growth_rate)) %>%
 ggplot() +
 aes(x = GDP_growth_rate, fill = sub_region) +
 geom_density() +
 scale_fill_brewer(palette = "Accent", direction = 1) +
 theme_minimal()

# example 4 example 5 ....



# linear regression with dummy variable
unique(lm_df_lag$region)
lm_df_lag %>% filter(year == 2022) %>%
              lm(log(TO_export) ~ log(TO_export_pre) + 
                                  GDP_growth_rate + 
                                  GDP_gr_rate_pre + 
                                  Inflation_rate + 
                                  Inflation_rate_pre + 
                                  region, data=.) %>% summary()

lm_df_lag %>% filter(year == 2022) %>% 
              mutate(region_f = factor(region, levels = c('Asia','Europe','Americas','Africa','Oceania'))
                     ) %>%
              lm(log(TO_export) ~ log(TO_export_pre) + 
                                  GDP_growth_rate + 
                                  GDP_gr_rate_pre + 
                                  Inflation_rate + 
                                  Inflation_rate_pre + 
                                  region_f, data=.) %>% summary()



