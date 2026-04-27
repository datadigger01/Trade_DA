library(ggplot2)
library(dplyr)
library(patchwork) # 그래프 병합용

# 1. 가상 패널 데이터 생성 (3개 국가, 10년간 데이터)
set.seed(123)
data <- data.frame(
  year = rep(1:10, 3),
  country = rep(c("A", "B", "C"), each = 10),
  x = c(1:10, 1:10 + 2, 1:10 + 5),
  # 각 국가는 서로 다른 절편을 가짐 (A=20, B=10, C=0)
  intercept = rep(c(20, 10, 0), each = 10)
)
# 실제 관계는 y = 1.5 * x + intercept + noise
data$y <- 1.5 * data$x + data$intercept + rnorm(30, sd = 2)

# 2. 시각화 A: Pooled OLS (고정효과 무시)
p1 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "1. Pooled OLS", 
       subtitle = "국가별 차이를 무시하면 기울기가 왜곡됨 (음의 관계처럼 보임)") +
  theme_minimal()

# 3. 시각화 B: Fixed Effects (국가별 절편 허용)
p2 <- ggplot(data, aes(x = x, y = y, color = country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "2. Fixed Effects (Within)", 
       subtitle = "국가별 고유 특성(절편)을 통제하면 진정한 양(+)의 관계가 보임") +
  theme_minimal()

# 그래프 나란히 배치
p1 + p2