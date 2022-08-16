df <- mtcars
str(df)
df <-  mtcars[, 1:6]
str(df)

plot(df, col = 'green', pch ='+')

cor(df)

install.packages('corrgram')
library(corrgram)
corrgram(df)


# 결정계수 R^2 선형회귀식의 설명력 지표
# R^2 = 0 : 독립변수와 종속변수 간의 선형 관계가 존재하지 않음
# R^2 = 1 : 독립변수와 종속변수 간의 선형 관계가 존재함
model <- lm(mpg ~., data = df)
summary(model)


# Adjusted R^2
# R^2은 독립변수의 개수가 증가하면 항상 값이 증가하니까 이 부분을 조정함

# AIC 지표
# 일반적으로 AIC 값이 작을수록 더 적은 개수의 파라미터로 적절한 적합도를 달성하고 있음

# mtcars 데이터셋에서 후진서택법으로 회귀모델 구축
mtcars.lm <-  lm(mpg ~ hp + wt + disp + drat, data = mtcars)
mtcars.lm
mod.selected <- step(mtcars.lm, direction='backward')
summary(mod.selected)

# 연습문제
# Kaggle House Price 데이터셋에서
# 다중 선형 회귀의 변수 선택을 통해
# 최적의 독립 변수 조합을 찾아보시오.
# 1. 전진선택법으로 찾은 조합은? R^2, Adjusted R^2 값은?
# 2. 후진선택법으로 찾은 조합은? R^2, Adjusted R^2 값은?

# 데이터 불러오기
data.house <- read.csv('./House_Price.csv')

str(data.house)

# 수치형 데이터만 추출
is.num <-  c()
for (i in 1:80) {
  is.num[i] <- is.numeric(data.house[,i])
}
data.house_num <-  data.house[,is.num]
data.house_num <-  data.house_num[, -1]
data.house_num <-  data.house_num[complete.cases(data.house_num),]
data.house_num.lm <- lm(SalePrice ~ ., data = data.house_num)

# 1. 전진선택법으로 찾은 조합은? R^2, Adjusted R^2 값은?
data.house.lm.front <- step(data.house_num.lm, direction='forward')
summary(data.house.lm.front)


# 2. 후진선택법으로 찾은 조합은? R^2, Adjusted R^2 값은?
data.house.lm.back <- step(data.house_num.lm, direction='backward')
summary(data.house.lm.back)

df <- InsectSprays
df
model <- lm(count ~ spray, data = df)
summary(model)

contrasts(df$spray)

df <-  mtcars[, 1:6]

df$cyl <- factor(df$cyl)
table(df$cyl)

lm(mpg ~., data = df)
model <- lm(mpg ~., data = df)
summary(model)
