
# setwd("")
df1 <- read.csv("./wdi_data.csv")
df2 <- read.csv("./country_region_code.csv")


# left join 
left_join(df1, df2, by=c("iso3c"="alpha.3")) %>% view()


df1_1 <- df1 %>% filter(!region %in% c("","Aggregates")) %>%
                select(country:year, GDP_PER_CAPITA:region)
names(df2)
df2_1 <- df2 %>% select(alpha.3, region, sub.region, intermediate.region) %>% 
                  rename(iso3c = alpha.3,
                         region_new = region,
                         sub_region_new = sub.region,
                         inter_region_new = intermediate.region)


df12_join <- left_join(df1_1, df2_1, by=c("iso3c" = "iso3c"))
# df12_join <- df1_1 %>% filter(!region %in% c("","Aggregates")) %>%
#                        select(country:year, GDP_PER_CAPITA:region) %>% 
#                   left_join(df2 %>% select(alpha.3, region, sub.region, intermediate.region) %>% 
#                                     rename(iso3c = alpha.3,
#                                             region_new = region,
#                                             sub_region_new = sub.region,
#                                             inter_region_new = intermediate.region),
#                             by=c("iso3c"="iso3c")
#                             )

# t-test using %>% operation
unique(df12_join$sub_region_new)

df12_join %>% filter(sub_region_new %in% c("Latin America and the Caribbean","Western Asia") 
              & year == 2020) %>%
  select(country, iso3c, sub_region_new, GDP_GROWTH_RATE) %>% 
  drop_na(GDP_GROWTH_RATE) %>% 
  t.test(GDP_GROWTH_RATE ~ sub_region_new, data=.)







df3 <- read.csv("./gdp_by_OECD_country.csv")
# inner join
df1 %>% 
  select(country:year, GDP_PER_CAPITA:region) %>%
  inner_join( df3 %>% select(LOCATION, Time, Value, Category) %>% 
                      pivot_wider( names_from = "Category", names_glue = "{Category}_LCU",values_from = "Value")
             , by=c("iso3c"="LOCATION", "year"="Time")
             ) -> combin_result


new_df <- combin_result %>% mutate(EXP_RT_INGDP = EXPORT_LCU / (GDP_LCU * 1000000),
                                   KOR_YN       = iso3c=="KOR" ) %>% 
                            select(country:year, KOR_YN, EXP_RT_INGDP)

new_df %>% drop_na(EXP_RT_INGDP) %>% 
          ggplot(aes(x=year, y=EXP_RT_INGDP, color=KOR_YN, group=country)) + 
          geom_line() +
          scale_color_manual(breaks = c(FALSE, TRUE),
                             values = c("gray","red"))




# country_info <- df %>% distinct(country, iso2c, iso3c, region) %>%
#                     filter(!region %in% c("","Aggregates"))
# gdp_info <- gdp_df %>% filter(MEASURE=="V") %>% 
#   select(LOCATION, ObsValue, TRANSACT, Time, UNIT) %>% 
#   mutate(Value = as.numeric(ObsValue)
#          ,Time  = as.numeric(Time)
#          ,Category = case_when(
#                       TRANSACT == "B1_GA" ~ "GDP",
#                       TRANSACT == "P6" ~ "EXP_GDS_SRVS",
#                       TRANSACT == "P7" ~ "IMP_GDS_SRVS",
#                       .default = as.character(TRANSACT)
#                       )
#          ) %>% 
#   select(-ObsValue, -TRANSACT) 