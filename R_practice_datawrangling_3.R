
df1 <- read.csv("./wdi_data.csv")
df2 <- read.csv("./gdp_by_OECD_country.csv")


# left join
left_join(df1, df2, by=c("iso3c"="LOCATION"))

df1 %>% filter(!region %in% c("","Aggregates")) %>% 
        select(country:year, GDP_PER_CAPITA:region) %>% view()

df2 %>% select(LOCATION, Time, Value, Category) %>% 
        pivot_wider( names_from = "Category", names_prefix = "LCU_",values_from = "Value") %>% view()


df1 %>% filter(!region %in% c("","Aggregates")) %>% 
  select(country:year, GDP_PER_CAPITA:region) %>%
  left_join(df2 %>% select(LOCATION, Time, Value, Category) %>% 
                    pivot_wider( names_from = "Category", names_prefix = "LCU_",values_from = "Value")
            ,by=c("iso3c"="LOCATION","year"="Time")
  ) %>% view()




# inner join
df1 %>% filter(!region %in% c("","Aggregates")) %>% 
  select(country:year, GDP_PER_CAPITA:region) %>%
  inner_join(df2 %>% select(LOCATION, Time, Value, Category) %>% 
              pivot_wider( names_from = "Category", names_prefix = "LCU_",values_from = "Value")
            ,by=c(iso3c="LOCATION",year="Time")
  ) -> combin_result



combin_result %>% filter(iso3c=="KOR") %>% view()



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