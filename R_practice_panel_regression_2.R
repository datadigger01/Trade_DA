
#library('tidyverse')

df <- read.csv("./final_merged_df.csv")


# select countries that it's population number has more than 5 mil population
df_over5m <- 

# generate new column : corona_yn
df_over5m_lm <- df_over5m %>% mutate(corona_yn = case_when(year >= 2020 & year <= 2021 ~ "Y", 
                                                                                  TRUE ~ "N")) %>% 
                              drop_na(TO_export, TO_export_pre, GDP_growth_rate, GDP_gr_rate_pre, 
                                      Inflation_rate, Inflation_rate_pre, population, gdp_per_capita)
                                                           

# pooling OLS
pooledOLS_result <- lm(data=df_over5m_lm, log(TO_export) ~ log(TO_export_pre) + 
                                                            GDP_growth_rate + 
                                                            GDP_gr_rate_pre +
                                                            Inflation_rate +
                                                            Inflation_rate_pre +
                                                            log(population) +
                                                            log(gdp_per_capita) +
                                                            corona_yn
                      )
summary(pooledOLS_result)
# check residual
plot(pooledOLS_result,1)



# heterogeneity checking.
lm_lsdv <- lm(data=df_over5m_lm, log(TO_export) ~ log(TO_export_pre) + 
                                           GDP_growth_rate + 
                                           GDP_gr_rate_pre +
                                           Inflation_rate +
                                           Inflation_rate_pre +
                                           log(population) +
                                           log(gdp_per_capita) +
                                           factor(iso3c)
                                           # factor(year)
              )
summary(lm_lsdv)
#names(lm_lsdv)

plot(lm_lsdv,1)



# "plm" : panel linear regressin package in R
install.packages("plm")
library("plm")


# 0. simple poolingOLS using plm
plm_pool <-  plm(data=df_over5m_lm, log(TO_export) ~ log(TO_export_pre) + 
                                     GDP_growth_rate +
                                     GDP_gr_rate_pre +
                                     Inflation_rate +
                                     Inflation_rate_pre +
                                     log(population) +
                                     log(gdp_per_capita) +
                                     corona_yn,
                                   index = c('iso3c', 'year'),
                                   model = 'pooling'
                )
summary(plm_pool)


# table format with linear model result.
if (!require(stargazer)) install.packages('stargazer')
library(stargazer)

stargazer(pooledOLS_result, plm_pool, type='text', title='Comparison OLS & plm OLS')


####################################################################################
#    
#                Fixed Effects Model
#
####################################################################################
# 1. fixed effect model with WG(within group) option
plm_fx_wi <- plm(data=df_over5m_lm, log(TO_export) ~ log(TO_export_pre) + 
                                                     GDP_growth_rate +
                                                     GDP_gr_rate_pre +
                                                     Inflation_rate +
                                                     Inflation_rate_pre +
                                                     log(population) +
                                                     log(gdp_per_capita),
                                                     # factor(year),
                              index = c('iso3c', 'year'),
                              model = 'within'
                  )
summary(plm_fx_wi)
#names(plm_fx_wi)
# fixed effect by country ( constants for each country)
#fixef(plm_fx_wi)


stargazer(lm_lsdv, plm_fx_wi,  type = 'text')
stargazer(pooledOLS_result, lm_lsdv, plm_fx_wi,  type = 'text')


# fixed effect model with FD(first difference) option
plm_fx_fd <- plm(data=df_over5m_lm, log(TO_export) ~ log(TO_export_pre) + 
                   GDP_growth_rate +
                   GDP_gr_rate_pre +
                   Inflation_rate +
                   Inflation_rate_pre +
                   log(population) +
                   log(gdp_per_capita),

                 index = c('iso3c', 'year'),
                 model = 'fd',
                )
summary(plm_fx_fd)




# plmtest(plm_fx_wi, c("time"), type = ("bp"))
# plmtest(plm_fx_wi, type = ("bp"))


####################################################################################
#    
#                Random Effects Model
#
####################################################################################
# random effect
p_result_ran<- plm(data=df_over5m_lm, log(TO_export) ~ log(TO_export_pre) + 
                                             GDP_growth_rate +
                                             GDP_gr_rate_pre +
                                             Inflation_rate +
                                             Inflation_rate_pre +
                                             log(population) +
                                             log(gdp_per_capita),
                    index = c('iso3c', 'year'),
                    model = 'random'
                   )
summary(p_result_ran)

##################################################################################
# Houseman test
##################################################################################
phtest(plm_fx_wi,  p_result_ran)



# Durbin-Watson test
# pdwtest(plm_fx_wi)