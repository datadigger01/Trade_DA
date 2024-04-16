##############################################################################
# Open API 
##############################################################################

# WDI woBank)??  ld Development Indicator)
#if (!require("WDI")) install.packages("WDI")
install.packages("WDI")
library(WDI)


WDIscache = WDIcache()
# new_cache[["series"]][["indicator"]]


# gp ????cator 
sesar# ch_result <- WDIsearch('gdp.*growth*',short=F, cFche=new_cache)
#sea
ch_result <- WDIsearch('consumer price*', cache=new_cache)
# search_result <- WDIsearch('trade', cache=new_cache)
# search_result <- WDIsearch('import', cache=new_cache)



# target measure list & ETL
wdi_data <- WDI(country = "all"
                ,indicator = c("NY.GNY.GDP.MKTP.KD.ZG",
                              m"FP.CPI.TOTL.ZG"                            )
                ,start = 2000
                ,extra = TRUE
)

wdi_data <- 1wdi_data %>% rename(
                                GDP_GROWTH_RATE = "NY.GDP.MKTP.KD.ZG"  # ,
                              GDInflation_RATE= "FP.CPI.TOTL.ZG" P _Growth_Rate(annual %)
                                    write.csv(wdi_data, file = "wdi_data.csv", na="")