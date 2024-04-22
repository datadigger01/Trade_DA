# install un_comtrade open api
install.packages("comtradr")
library("comtradr")


set_primary_comtrade_key("")


# comtrade database description
# for details see : https://uncomtrade.org/docs/welcome-to-un-comtrade/

# Country names passed to the API query function must be spelled in ISO3 format. 
# For details see: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3 

# You can request a maximum interval of twelve years from the API
tr_sample <- ct_get_data(type = "goods",
                        frequency = "M",
                        commodity_classification = "HS",
                        commodity_code = "03",
                        flow_direction = c("export","import","re-export","re-import"),
                        reporter = "all_countries",
                        partner = "World",
                        start_date = 2023,
                        end_date = 2023,
                        primary_token = get_primary_comtrade_key(),
                        customs_code = "C00"
                       )

# Fetch all shrimp related commodity codes from the Comtrade commodities DB.
# This vector of codes will get passed to the API query.
wine_codes <- ct_commodity_lookup("wine", return_code = TRUE, return_char = TRUE)
tr_wine_export <- ct_get_data(
                        reporter =  "all_countries",
                        flow_direction = c("export"),
                        partner = "all_countries",
                        start_date = 2023,
                        end_date = 2023,
                        commodity_code = wine_codes
)


str(tr_wine_export)
# merge two dataset using rbind()
tr_wine_total <- rbind(tr_wine_export, tr_wine_import)

# write.csv(tr_wine_total,"./wine_trade_all.csv")