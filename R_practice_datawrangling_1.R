

##### Datawrangling with tidyverse #########
# www.tidyverse.org 사이트 참고


# 1. install 
install.packages("tidyverse")     # Tool 메뉴에 install package를 이용해도됨
library(tidyverse)                # file/plots/package etc 창에서 package에서 선택해도 활성됨


# data import 
# getwd(), setwd()
# 엑셀인 경우, tidyverse 에서 불러오기 예시 
## --> df1 <- readxl::read_excel("./Life_Exp_by_Country.xlsx")  ::은 library()로 로드되지 않지만 패기지에 포함된 함수 사용법

df <- read.csv("./wdi_data.csv")

# 1. 기본 탐색 
# %>% : tidyverse에서 사용하는 파이프(pipe), control+shift+M(windows), cmd+shift+M(macOS)
df %>% head(n=3)
df %>% tail(n=3)
df %>% names()
df %>% glimpse()
df %>% view()
df %>% drop_na()


unique(df$iso3c)
unique(df$region)

# 2. 행(row) 제어
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


# 3. 열(column) 제어
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
df %>% filter(!region %in% c("","Aggregates") & year >= 2020) %>%
  group_by(region, country) %>%
  summarise(n=n(),
            TRADE_R=mean(GDP_GROWTH_RATE, na.rm=T)) %>% 
  slice_max(TRADE_R, n=3) %>% print(n=30)

