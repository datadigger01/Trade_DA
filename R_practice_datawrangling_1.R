

##### Datawrangling with tidyverse #########
# www.tidyverse.org


# 1. install 
install.packages("tidyverse")     # also install this package install package in tool bar menu
library(tidyverse)                # check the selection bar in file/plots/package etc  package


# data import 
# getwd(), setwd()
# in the case of excel file, refer to the below code.
## df1 <- readxl::read_excel("./Life_Exp_by_Country.xlsx")  
## :: -> use this character, when you want to use in not active package 

setwd("D:/성균관대학교/강의/2024/무역투자와데이터분석/데이터분석/실습데이터/Data")
df <- read.csv("./wdi_data.csv")

# 1. Basic data exploration
# %>% :  pipe in tidyvesre, short key : control+shift+M(windows), cmd+shift+M(macOS)
df %>% head(n=3)
df %>% tail(n=3)
df %>% names()
df %>% glimpse()
df %>% view()
df %>% drop_na()


unique(df$iso3c)
unique(df$region)

# 2. row(record) control
# filter
df %>% filter(iso3c=="KOR")
df %>% filter(iso3c=="KOR"|iso3c=="JPN"|iso3c=="CHN") %>% view()
df %>% filter(iso3c %in% c("KOR","JPN","CHN")) %>% glimpse()
df %>% filter(region=='East Asia & Pacific') %>% view()
df %>% filter(GDP_PER_CAPITA > 0) %>% view()
df %>% filter(GDP_PER_CAPITA > 0 & GDP_GROWTH_RATE > 0) %>% view()

# slice
df %>% filter(iso3c=="KOR") %>% view()
df %>% filter(iso3c=="KOR") %>% slice(1:3)
df %>% filter(iso3c=="KOR") %>% slice(20:n()) %>% view()
df %>% filter(iso3c=="KOR") %>% slice_max(GDP_GROWTH_RATE, n=1) %>% view()
df %>% filter(iso3c %in% c("KOR","JPN","CHN")) %>% slice_min(GDP_GROWTH_RATE) %>% view()
df %>% filter(region=='East Asia & Pacific') %>% slice_max(GDP_GROWTH_RATE) %>% view()

# distinct
df %>% distinct(iso3c)
df %>% distinct(iso3c, region)
df %>% distinct(iso3c, region) %>% count()
df %>% distinct(country, iso2c, iso3c, region) %>%
        filter(!region %in% c("","Aggregates")) %>% view()


# 3. column control
# select
names(df)
df %>% select(country, iso2c, iso3c, year) %>% view()
df %>% select(country:year)
df %>% select(country:year, GDP_PER_CAPITA:region) %>% view()

# mutate
df %>% filter(iso3c %in% c("KOR","JPN","CHN")) %>% 
        select(country:year, GDP_PER_CAPITA:region) %>%
        mutate(EXP_IMP_LCU_R = EXPORT_LCU/IMPORT_LCU
              ,EXP_IMP_USD_R = round(EXPORT_USD/IMPORT_USD, 2)
              ,PLUS_LCU_BOP = EXPORT_LCU > IMPORT_LCU) %>% view()

# rename
df %>% filter(iso3c %in% c("KOR","JPN","CHN")) %>% 
  select(country:year, GDP_PER_CAPITA:region) %>%
  mutate(EXP_IMP_LCU_R = EXPORT_LCU/IMPORT_LCU
         ,EXP_IMP_USD_R = round(EXPORT_USD/IMPORT_USD, 2)
         ,PLUS_LCU_BOP = EXPORT_LCU > IMPORT_LCU) %>% 
  rename(TIME=year
         ,COUNTRY_3 = iso3c) %>% view()


# relocate
df %>% filter(iso3c %in% c("KOR","JPN","CHN")) %>% 
        select(country:year, GDP_PER_CAPITA:region) %>%
        mutate(EXP_IMP_LCU_R = EXPORT_LCU/IMPORT_LCU
              ,EXP_IMP_USD_R = round(EXPORT_USD/IMPORT_USD, 2)
              ,PLUS_LCU_BOP = EXPORT_LCU > IMPORT_LCU) %>% 
        rename(TIME=year
              ,COUNTRY_3 = iso3c) %>%
        relocate(TIME, COUNTRY_3, country,region
                 ,GDP_PER_CAPITA, GDP_GROWTH_RATE, TRADE_IN_GDP, everything()) %>% view()



# group by & summarise
df %>% filter(!region %in% c("","Aggregates") & year==2020) %>%
        group_by(region) %>% 
        summarise(cnt = n(),
               avg_growth_r = mean(GDP_GROWTH_RATE, na.rm = T),
               sd_growth_r = sd(GDP_GROWTH_RATE, na.rm = T),
               median_growth_r = median(GDP_GROWTH_RATE, na.rm=T),
               percentile_25 = quantile(GDP_GROWTH_RATE, prob=0.25, na.rm=T),
               percentile_75 = quantile(GDP_GROWTH_RATE, prob=0.75, na.rm=T)
               )

# arrange
df %>% filter(!region %in% c("","Aggregates") & year==2020) %>%
  group_by(region) %>% 
  summarise(cnt = n(),
            avg_growth_r = mean(GDP_GROWTH_RATE, na.rm = T),
            sd_growth_r = sd(GDP_GROWTH_RATE, na.rm = T),
            median_growth_r = median(GDP_GROWTH_RATE, na.rm=T),
            percentile_25 = quantile(GDP_GROWTH_RATE, prob=0.25, na.rm=T),
            percentile_75 = quantile(GDP_GROWTH_RATE, prob=0.75, na.rm=T)) %>% 
  arrange(avg_growth_r)


# ETC
# unique(df$year)
# extract three countries by region showing the lowest GDP growth rate in 2020(covid19 outbreak)

# 1
df %>% filter(!region %in% c("","Aggregates") & year == 2020) %>%
  select(country, region, GDP_GROWTH_RATE) %>% 
  group_by(region) %>% 
  slice_min(GDP_GROWTH_RATE, n=3) %>% view()

# 2
df %>% filter(!region %in% c("","Aggregates") & year == 2020) %>%
  group_by(region, country) %>%
  summarise(n=n(),
            TRADE_R=mean(GDP_GROWTH_RATE, na.rm=T)) %>%
  slice_min(TRADE_R, n=3) %>% print(n=30)




unique(df$region)
## t-test
## verification the difference in the average GDP growth rate between two regions using t-test
df %>% filter(region %in% c("Latin America & Caribbean","Europe & Central Asia")
              & year == 2020) %>%
  select(country, iso3c, region, GDP_GROWTH_RATE) %>% 
  drop_na(GDP_GROWTH_RATE) -> ttest_df

t.test(ttest_df$GDP_GROWTH_RATE ~ ttest_df$region)
# t_test_result <- t.test(ttest_df$GDP_GROWTH_RATE ~ ttest_df$region)
# t_test_result$method

# t-test using %>% operation 
df %>% filter(region %in% c("Latin America & Caribbean","Europe & Central Asia") 
              & year == 2020) %>%
       select(country, iso3c, region, GDP_GROWTH_RATE) %>% 
       drop_na(GDP_GROWTH_RATE) %>% 
       t.test(GDP_GROWTH_RATE ~ region, data=.)

#  distribution by region with ggplot()
df %>% filter(region %in% c("Latin America & Caribbean","Europe & Central Asia") 
              & year == 2020) %>%
  select(country, iso3c, region, GDP_GROWTH_RATE) %>% 
  drop_na(GDP_GROWTH_RATE) %>%
  ggplot(data = .,aes(x=GDP_GROWTH_RATE,fill=region)) + 
  geom_density(alpha=0.8) +
  # geom_vline(xintercept = -4.486581, color='red') +
  # geom_vline(xintercept = -9.304592, color='blue') +
  theme_bw()

# ANOVA test
df %>% filter(region %in% c("South Asia","Latin America & Caribbean","Europe & Central Asia")
              & year == 2020) %>% 
      select(country, iso3c, region, GDP_GROWTH_RATE) %>% 
      drop_na(GDP_GROWTH_RATE) %>% 
      aov(GDP_GROWTH_RATE ~ region, data=.) %>% summary()
  
# result <- aov(GDP_GROWTH_RATE ~ region, data=anov_t)
# summary(result)
df %>% filter(region %in% c("South Asia","Latin America & Caribbean","Europe & Central Asia")
              & year == 2020) %>% 
  select(country, iso3c, region, GDP_GROWTH_RATE) %>% 
  drop_na(GDP_GROWTH_RATE) %>%
  ggplot(data = .,aes(x=GDP_GROWTH_RATE,fill=region)) + 
  geom_density(alpha=0.8) +
  theme_bw()
