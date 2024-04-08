library(tidyverse)
library(ggplot2)

df <- read.csv("./WtoData_20240327050855.csv")

# column selection and renaming
df %>% select(Indicator.Code, 
              Reporting.Economy.ISO3A.Code,
              Reporting.Economy,
              Product.Sector.Code,
              Product.Sector,
              Unit.Code,
              year = Year,
              value = Value
) %>% 
  rename(Ind_cd =Indicator.Code,
         iso3c  = Reporting.Economy.ISO3A.Code,
         country = Reporting.Economy,
         prod_cd = Product.Sector.Code,
         prod_ds = Product.Sector,
         unit = Unit.Code,
        ) %>% 
  mutate( EXP_IMP = case_when( Ind_cd == "ITS_MTP_AMF" ~ "import",
                               Ind_cd == "ITS_MTP_AXF" ~ "export"
                               )) -> df1

# pivot_wider()
df1 %>% group_by(prod_cd, prod_ds) %>% count()

df2 <- df1 %>% select(iso3c, country, year, prod_cd, EXP_IMP, value) %>%
                pivot_wider(names_from = c(prod_cd, EXP_IMP), values_from = value) %>%
                arrange(iso3c, year)


## add region info to df2
region_info <- read.csv("./country_region_code.csv")
df3 <- region_info %>% select(alpha.3, region, sub.region, intermediate.region) %>% 
                       rename(iso3c = alpha.3, sub_region = sub.region, inter_region=intermediate.region) %>% 
                    inner_join(., df2, by="iso3c") %>%
                    relocate(country, iso3c, region, sub_region, inter_region, year, TO_export, TO_import)

# load the gdp growth rate and inflation rate(cusumer price index) file from world bank data
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


# merge these two files with export & import df
left_join(df3, gdp_grw_f, by=c("iso3c"="iso3c","year"="year")) %>%
  left_join(., info_cpi_f, by=c("iso3c"="iso3c", "year"="year")) -> final_df


unique(final_df$region)
final_df %>% filter(year >= 2022 & GDP_growth_rate > -10 & !sub_region %in% c("Melanesia","Polynesia","Micronesia")) %>%
              # ggplot(aes(x=GDP_growth_rate, y=log(TO_export), group=region, color=region)) +
              ggplot(aes(x=GDP_growth_rate, y=log(TO_export))) +
              geom_point() +
              # facet_wrap(~region, ncol=3) +
              # geom_smooth(method = "lm", se=F) +
              theme_bw()

# lm_df <- final_df %>% filter(year == 2022 & GDP_growth_rate > -10 & region %in% c("Asia"))
lm_df <- final_df %>% filter(year == 2022 & GDP_growth_rate > -10 & !sub_region %in% c("Melanesia","Polynesia","Micronesia"))
# linear regression
lm_model <- lm(log(TO_export) ~ GDP_growth_rate, data=lm_df)
summary(lm_model)


# graph with linear regression result using ggplot
ggplot(data = lm_df, aes(x=GDP_growth_rate, y=log(TO_export))) +
  geom_point() +
  # geom_smooth(method =lm, se=F) +
  geom_abline(intercept = 4.962797, slope = 0.016169)

  

# linear_regression between gdp_growth_rate and log(to_export) in Asisa area
############################################################################


