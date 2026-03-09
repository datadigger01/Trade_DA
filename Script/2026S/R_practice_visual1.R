library(tidyverse)
library(ggplot2)

df <- read.csv("./WtoData_20240327013640.csv")

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
        mutate(EXP_IMP = if_else(Ind_cd=="ITS_MTV_AM","import","export")) -> df1



df1 %>% group_by(prod_cd, prod_ds) %>% count()
# filtering the targeted row in product sector(4 types) and choose key columns 
#
#


# transpose values by product using pivot_wider
#
#


# filtering by country code with country_region_code.csv file using inner join
df_country_cd <- read.csv("./country_region_code.csv")
inner_join(     
            ,by=c( "iso3c"="iso3c" )
          )


# derive new variables
# generate new variables : ratio AG, ratio MA and ratio MI of Total EXP, IMP






##############################################################################
# Visualization with ggplot2 package
##############################################################################
sample_dt <- df4 %>% filter(year==2022) %>% 
                      drop_na(ratio_AG_exp, ratio_MA_exp, ratio_AG_imp, ratio_MA_imp)

# Canvas
ggplot(data = sample_dt) + theme_bw()


#################################
# Boxplot(geom_boxplot) in ggplot
#################################

# ex1 
# unique(sample_dt$sub_region)
ggplot(data = sample_dt) +
  geom_boxplot(mapping = aes(x=sub_region, y=ratio_AG_exp)) +
  theme_classic()
# ex2
ggplot(data = sample_dt) +
  geom_boxplot(mapping = aes(x=sub_region, y=ratio_AG_exp)) +
  coord_flip() +
  theme_classic()
# ex3
df4 %>% filter(year==2022) %>% drop_na(ratio_AG_exp) %>% 
        ggplot() +
        geom_boxplot(mapping = aes(x=sub_region, y=ratio_AG_exp)) +
        coord_flip() +
        theme_bw()
# ex4
df4 %>% filter(iso3c %in% c("CHN","KOR","JPN")) %>% 
        drop_na(ratio_MA_imp) %>% 
          ggplot() +
          geom_boxplot(mapping = aes(x=country, y=ratio_MA_imp)) +
          coord_flip() +
          theme_bw()

#################################
# density(geom_density) in ggplot
#################################
df4 %>% filter(iso3c %in% c("CHN","KOR","JPN") & year >= 2000) %>% 
  drop_na(ratio_MA_exp) %>% 
  ggplot(aes(x=ratio_MA_exp, color=country)) +
  geom_density(linewidth=1) +
  theme_bw()




#######################################
# scatter plot(geom_point) in ggplot
#######################################

# ex1
df4 %>% filter(year==2022) %>% drop_na(ratio_AG_exp, ratio_AG_imp) %>% 
          ggplot() +
          geom_point(mapping = aes(x=ratio_AG_imp, y=ratio_AG_exp)) +
          theme_classic()
# ex1
df4 %>% filter(year==2022) %>% drop_na(ratio_AG_exp, ratio_AG_imp) %>% 
          ggplot() +
          geom_point(mapping = aes(x=ratio_AG_imp, y=ratio_AG_exp, color=region)) +
          theme_classic()
# ex1
df4 %>% filter(year==2022 & TO_export >= 1000) %>% drop_na(ratio_MA_exp, ratio_MA_imp) %>% 
          ggplot() +
          geom_point(mapping = aes(x=ratio_MA_imp, y=ratio_MA_exp, color=region)) +
          theme_classic()


##################################
# line(geom_line) in ggplot
##################################
unique(df4$sub_region)
# ex1
df4 %>% filter(region %in% c("Asia")) %>% drop_na(ratio_MA_exp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_AG_exp)) +
  theme_classic()

# ex2
df4 %>% filter(region %in% c("Asia")) %>% drop_na(ratio_MA_exp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_AG_exp, group=iso3c)) +
  theme_classic()

# ex3
df4 %>% filter(iso3c %in% c("CHN","KOR","JPN")) %>% drop_na(ratio_MA_exp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_MA_exp, group=iso3c)) +
  theme_classic()

# ex4
df4 %>% filter(iso3c %in% c("CHN","KOR","JPN")) %>% drop_na(ratio_MA_exp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_MA_exp, group=iso3c, color=iso3c)) +
  theme_classic()

# ex5
df4 %>% filter(iso3c %in% c("CHN","KOR","JPN")) %>% drop_na(ratio_MA_imp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_MA_imp, group=iso3c, color=iso3c), linewidth=1) +
  theme_classic()

  

##################################
# facet_wrap, facet_grid in ggplot
##################################
df4 %>% drop_na(ratio_MA_exp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_MA_exp, group=iso3c)) +
  theme_classic()

unique(df4$sub_region)
df4 %>% filter(!sub_region %in% c("Micronesia","Polynesia","Melanesia")) %>% 
        drop_na(ratio_MA_exp) %>% 
  ggplot() +
  geom_line(mapping = aes(x=year, y=ratio_MA_exp, group=iso3c)) +
  facet_wrap(~sub_region) +
  theme_classic()
