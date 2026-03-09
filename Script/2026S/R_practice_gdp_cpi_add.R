# load the gdp growth rate and inflation rate(cusumer price index) file from world bank data
gdp_grw <- read_csv("./API_NY.GDP.MKTP.KD.ZG_DS2.csv", col_names = T, skip = 0)
inf_cpi <- read_csv("./API_FP.CPI.TOTL.ZG_DS2.csv", col_names = T, skip = 0)

gdp_grw %>% select("Country Code", "2000":"2023") %>% 
  pivot_longer( cols = "2000":"2023", names_to = "year", values_to = "GDP_growth_rate") %>%
  mutate(year = as.numeric(year)) %>% 
  rename(iso3c="Country Code") -> gdp_grw_f

inf_cpi %>% select("Country Code", "2000":"2023") %>% 
  pivot_longer( cols = "2000":"2023", names_to = "year", values_to = "Inflation_rate") %>% 
  mutate(year = as.numeric(year)) %>% 
  rename(iso3c="Country Code") -> info_cpi_f


# merge these two files with export & import df
# previous generated dataframe --> df4
left_join( df4, gdp_grw_f, by=c("iso3c"="iso3c","year"="year")) %>%
  left_join( info_cpi_f, by=c("iso3c"="iso3c", "year"="year")) -> final_df


unique(final_df$region)
final_df %>% filter(year >= 2010 & !sub_region %in% c("Melanesia","Polynesia","Micronesia")) %>%
  # filter(region %in% c("Americas")) %>% 
  ggplot(aes(x=GDP_growth_rate, y=log(TO_export), group=region, color=region)) +
  geom_point() +
  facet_wrap(~region, ncol=3) +
  geom_smooth(method = "lm", se=F)


