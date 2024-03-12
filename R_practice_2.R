############# R에서의 다양한 객체 #########################

### 1. 벡터(Vector)
price <- c(10, 13, 15)
typeof(price)

price[1]
price[2:3]
length(price)

quantity <- c(25, 3, 20)

price * quantity
expenditure <- price * quantity

sum(expenditure)
mean(expenditure)

total_exp <- sum(expenditure)
avg_exp <- mean(expenditure)


var <- c("price","quantity","total")
var[1]
var[2:3]
length(var)
typeof(var)

var1 <- factor(var)
var1
typeof(var1)

######## 주요 Tip ########## 
# Cntrl + L : console 창에 있는 내용 한꺼번에 지우기
# rm(특정객체이름) : 특정객체 지우기 


### 2. 행렬(Matrix)
# cbind(price, quantity, expenditure)
# rbind(price, quantity, expenditure)
cb <- cbind(price, quantity, expenditure)

matrix(data = cb, nrow = 3)
mat <- matrix(data = cb, nrow = 3)

mat[1,]
mat[,1]
mat[1:2,]
mat[1,2]


### 3. 데이터프레임(dataframe)
data.frame(cb)
data.frame(mat)

exp_data <- data.frame(price, quantity)
exp_data[1,]
exp_data[,1]
exp_data[,]

exp_data$price
exp_data$quantity

sum(exp_data$price)
mean(exp_data$price)
max(exp_data$price)
min(exp_data$price)
median(exp_data$price)


### 4. 리스트(List)
# 다양한 유형의 객체를 담을 수 있는 구조
list(price, quantity, expenditure, total_exp, var)
ls_data <- list(price, quantity, expenditure, total_exp, var)
ls_data[[1]]
ls_data[[4]]
ls_data[[5]]

ls_data[[5]][[1]]
ls_data[[1]][[2]]
