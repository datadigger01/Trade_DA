#install.packages("tidyverse")
library("tidyverse")

library("ggplot2")


# Load the data
# 방법 1: 기본 R 함수 사용
# wdi_data <- read.csv("wdi_data.csv")

# 방법 2: tidyverse (readr) 사용 - 권장
wdi_data <- read_csv("D:/성균관대학교/강의/2026/1학기/무역데이터분석/R/data/wdi_data.csv")

# 데이터 확인
head(wdi_data)
str(wdi_data)
glimpse(wdi_data)

