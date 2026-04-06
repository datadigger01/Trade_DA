#########################################################
library("tidyverse")
library("wbstats")
library("comtradr")

##### IMF API with R ####################################
install.packages("rsdmx")
library(rsdmx)

# agency and id identify the dataset you are interested in
flowref <- 'IMF.STA,ITG' # STA: International Financial Statistics (IFS), ITG: International Trade in Goods and Services

# filter identifies the subset of the dataset you want.
filter <- '.XG.FOB_USD.A' # XG: export of goods, FOB value, USD; A: annual data

# readSDMX() retrieves the data from the API and returns it as an SDMX object.
imf_data <- as.data.frame(readSDMX(providerId = 'IMF_DATA',
                                   resource = 'data',
                                   flowRef = flowref,
                                   key = filter,
                                   start = 2005, # set the time period: yearly -> yyyy, quarterly-> yyyy-Qn, monthly->yyyy-mm 
                                   end = 2025))
str(imf_data)
names(imf_data) <- tolower(names(imf_data)) # convert column names to lower case for easier handling

imf_data <- imf_data %>% 
                mutate( year = as.numeric(time_period),
                        export_goods_val = as.numeric(obs_value)) %>% 
                select(country, year, export_goods_val)
