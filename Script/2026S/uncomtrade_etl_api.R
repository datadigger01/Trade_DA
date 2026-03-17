# install.packages("tidyverse")
library("tidyverse")
library("dplyr")

# install un_comtrade open api
install.packages("comtradr")
library("comtradr")

# set primary comtrade key
set_primary_comtrade_key("#########################")


# country code
country_info <- country_codes
str(country_info)
# 삭제된 국가 정보는 제외
country_info <- country_info %>% filter(is.na(exit_year)==TRUE)

# reporter_info <- ct_get_ref_table("reporter")
# str(reporter_info)


# HS 코드 정보는 상품 분류 체계로, 국제 무역에서 사용되는 표준화된 코드입니다. 
# HS 코드는 6자리로 구성되어 있으며, 무역 통계 및 관세 분류에 사용됩니다.
# get_ref_table() 함수를 이용하여 HS 코드 정보를 가져올 수 있습니다.
hs_codes <- ct_get_ref_table("HS")
str(hs_codes)

# 2-digit: 2자리 코드는 상품의 대분류를 나타냅니다. 예를 들어, "01"은 "Live animals"를 나타냅니다.
hs2 <- hs_codes %>% filter(nchar(id) == 2) %>% filter(id != "99") %>% select(id, text)
# 4-digit
hs4 <- hs_codes %>% filter(nchar(id) == 4) %>% filter(id != "9999") %>% select(id, text, parent)
# 6-digit
hs6 <- hs_codes %>% filter(nchar(id) == 6) %>% filter(id != "999999") %>% select(id, text, parent)


# HS 코드정보에서 "wine"이라는 담어가 포함되어 있는 상품코드 추출 
# grep/grepl 함수 이용
grep("wine", hs_codes$text, ignore.case = TRUE)
hs_codes$text[grep("wine", hs_codes$text, ignore.case = TRUE)]
hs_codes$text[grep("\\bwine\\b", hs_codes$text, ignore.case = TRUE)]

# str_detect() 함수를 이용하여 "2204"이라는 단어가 포함된 품목 추출
# ^는 문자열의 시작을 나타내며, 2204는 찾고자 하는 문자열입니다. $는 문자열의 끝을 나타내는 것으로 2204$인 경우는 2204로 끝나는 대상
str_detect(hs6$text, "^2204")
hs6[str_detect(hs6$text, "^2204"),]
hs6[str_detect(hs6$text, "^2205"),]
hs_wine <- hs6[str_detect(hs6$text, "^2204") | str_detect(hs6$text, "^2205"),]

# ct_commodity_lookup() 함수를 이용하여 특정 상품에 대한 HS 코드를 검색할 수 있습니다. 
# 예를 들어, "wine"이라는 단어가 포함된 상품의 HS 코드를 검색하려면 다음과 같이 할 수 있습니다.
# ct_commodity_lookup("wine", return_code = TRUE, return_char = TRUE)

# ct_get_data() 함수를 이용하여 특정 기간 동안의 무역 데이터를 가져올 수 있습니다.
# You can request a maximum interval of twelve years from the API
wine_data1 <- ct_get_data(
            frequency = "A",
            commodity_classification = "HS",
            commodity_code = hs_wine$id,
            flow_direction = c("import"),
            reporter = c("KOR"),
            partner = "all_countries",
            start_date = 2020,
            end_date = 2025,
            primary_token = get_primary_comtrade_key()
            # type = "goods",
            # frequency = "M",
            # reporter = "all_countries"
            # partner = "World",
            # customs_code = "C00"
            # commodity_code = "03",
            #flow_direction = c("export","import","re-export","re-import"),
            )
wine_data2 <- ct_get_data(
        frequency = "A",
        commodity_classification = "HS",
        commodity_code = hs_wine$id,
        flow_direction = c("import"),
        reporter = c("KOR"),
        partner = "all_countries",
        start_date = 2010,
        end_date = 2019,
        primary_token = get_primary_comtrade_key()
        # type = "goods",
        # frequency = "M",
        # reporter = "all_countries"
        # partner = "World",
        # customs_code = "C00"
        # commodity_code = "03",
        #flow_direction = c("export","import","re-export","re-import"),
        )
wine_data3 <- ct_get_data(
      frequency = "A",
      commodity_classification = "HS",
      commodity_code = hs_wine$id,
      flow_direction = c("import"),
      reporter = c("KOR"),
      partner = "all_countries",
      start_date = 2005,
      end_date = 2009,
      primary_token = get_primary_comtrade_key()
      # type = "goods",
      # frequency = "M",
      # reporter = "all_countries"
      # partner = "World",
      # customs_code = "C00"
      # commodity_code = "03",
      #flow_direction = c("export","import","re-export","re-import"),
    )

# dataframe merge(결합)
wine_data <- bind_rows(wine_data3, wine_data2, wine_data1)
str(wine_data)
# 필요한 열만 선택하여 새로운 데이터프레임 생성
wine_data <- wine_data %>%
        select(c(ref_year, ref_month, reporter_iso, flow_desc, partner_iso, partner_code, 
                 cmd_code, cmd_desc, qty, net_wgt, cifvalue, fobvalue, primary_value))

# Column NAME 변경
names(wine_data) <- c("year", "month", "reporter", "flow", "partner", "partner_code",
                      "hs6", "hs6_desc", "qty", "net_wgt", "cifvalue", "fobvalue", "primary_value")
# 특정 Column 값 변경
# wine_data <- wine_data %>% replace(wine_data$reporter == "KOR", "Korea")


# transpose : spread() 함수를 이용하여 데이터프레임을 변환
# spread(wine_data, key = flow_desc, value = primary_value)


# 5개 국가 필터링 및 연도별 집계
wine_subset <- wine_data %>%
  filter(partner %in% c("AUS", "CHL", "ITA", "FRA", "USA", "ESP")) %>%
  group_by(year, partner) %>%
  summarise(total_net_wgt = sum(net_wgt, na.rm = TRUE), .groups = "drop")

# 선그래프 그리기
ggplot(wine_subset, aes(x = year, y = total_net_wgt, color = partner, group = partner)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(min(wine_subset$year), max(wine_subset$year), by = 1)) +  # 여기 추가
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "연도별 국가별 와인 순중량(Net Weight) 추이",
    x = "연도",
    y = "순중량 (kg)",
    color = "국가"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )
