

df <- read.csv("./wdi_data.csv")

# 1. pivot_wider()
df %>% filter(!region %in% c("","Aggregates")) %>%
       select(country:year, EXPORT_LCU) %>%
       arrange(country, year) %>%
    pivot_wider( names_from = year, values_from = EXPORT_LCU) %>% view()
      

df %>% filter(!region %in% c("","Aggregates")) %>%
  select(country:year, IMPORT_LCU) %>%
  arrange(country, year) %>%
  pivot_wider(names_from = year, names_prefix = "IMP_", values_from = IMPORT_LCU ) %>% view()


# 2. pivot_longer()
df %>% filter(region=='East Asia & Pacific') %>%
  select(country,iso3c, region, year, EXPORT_LCU, IMPORT_LCU) %>%
  arrange(country, year) %>%
  pivot_longer( cols = c(EXPORT_LCU, IMPORT_LCU), names_to = "EXP_IMP", values_to = "AMT_LCU") %>% view()


df %>% filter(region=='East Asia & Pacific') %>%
  select(country,iso3c, region, year, TRADE_IN_GDP:IMPORT_USD) %>%
  arrange(country, year) %>% 
  pivot_longer( cols = TRADE_IN_GDP:IMPORT_USD, names_to = "TRADE", values_to = "AMT") %>% view()


## boxplot graph after pivot_longer
df %>% filter(region=='East Asia & Pacific') %>%
  select(country,iso3c, region, year, EXPORT_LCU, IMPORT_LCU) %>%
  arrange(country, year) %>%
  pivot_longer( cols = c(EXPORT_LCU, IMPORT_LCU), names_to = "EXP_IMP", values_to = "AMT_LCU") %>% 
  drop_na(AMT_LCU) %>%  
  filter(year==2020) %>% 
  ggplot(aes(x=EXP_IMP, y=log(AMT_LCU))) + geom_boxplot()  ## boxplot



# 3. missing value 贸府 : drop
df %>% filter(!region %in% c("","Aggregates")) %>%
  select(country:year, EXPORT_LCU) %>%
  arrange(country, year) %>%
  pivot_wider( names_from = year, names_prefix = "EXP_", values_from = EXPORT_LCU ) %>% 
  drop_na()


# 3. missing value 贸府 : replace
df %>% filter(!region %in% c("","Aggregates")) %>%
  select(country:year, EXPORT_LCU) %>%
  arrange(country, year) %>%
  mutate(EXPORT_LCU = replace_na(EXPORT_LCU, 0)) %>%
  # replace_na(list(EXPORT_LCU=0)) %>%
  pivot_wider( names_from = year, names_prefix = "EXP_", values_from = EXPORT_LCU )
