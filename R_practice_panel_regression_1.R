
# merged dataset from three files
df <- read.csv("./final_merged_df.csv")


unique(df$sub_region)
df_lm <- df %>% drop_na(TO_export, TO_export_pre, GDP_growth_rate, GDP_gr_rate_pre, 
                        Inflation_rate, Inflation_rate_pre, population, gdp_per_capita) %>% 
                filter(!sub_region %in% c('Polynesia','Micronesia','Melanesia')) %>%
                mutate(corona_yn = case_when(year >= 2020 & year <= 2021 ~ "Y",
                                                                    TRUE ~ "N")) 
# pooling OLS
pooledOLS_result <- lm(data=df_lm, log(TO_export) ~ log(TO_export_pre) + 
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

# residual meaning
# plot(log(df_lm$TO_export_pre),log(df_lm$TO_export))
# lm_r <- lm(data=df_lm, log(TO_export) ~ log(TO_export_pre))
# abline(a=lm_r$coefficients[1], b=lm_r$coefficients[2])

# names(pooledOLS_result)
# check <- data.frame(cbind(df_lm$year, pooledOLS_result$fitted.values, pooledOLS_result$residuals))
# names(check) <- c('year','Fitted_V','Residual')
# check %>% ggplot() +
#           geom_point(aes(x=Fitted_V, y=Residual, color=year)) +
#           geom_hline(yintercept = 0, color='red', lwd=1) +
#           theme_bw()



# heterogeneity checking after applying panel linear regression.
lm_test <- lm(data=df_lm, log(TO_export) ~ log(TO_export_pre) + 
                                           GDP_growth_rate + 
                                           GDP_gr_rate_pre +
                                           Inflation_rate +
                                           Inflation_rate_pre +
                                           log(population) +
                                           log(gdp_per_capita) +
                                           iso3c
                                           # factor(year)
              )
summary(lm_test)
names(lm_test)
plot(lm_test,1)