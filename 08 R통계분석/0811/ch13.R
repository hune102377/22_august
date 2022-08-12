install.packages('HistData')
library(HistData)
df <-  GaltonFamilies
str(df)

# 독립변수 : midparentHeight 부모의 키
plot(df$midparentHeight, df$childHeight,
     main = "부모의 키와 자녀의 키 상관관계",
     xlab = "자녀의 키",
     ylab = '부모의 키',
     col = adjustcolor("cyan", alpha.f = 0.3),
     pch = 19)

# 선형회귀 분석을 해주는 함수 lm
model <-  lm(childHeight ~ midparentHeight, data = df)
abline(model,
       col = adjustcolor('tomato', alpha.f = 0.6),
       lty = 1,
       lwd = 3
       )

x <-  runif(n = 300, min = 0, max = 100)
y <-  3* x + 5
plot(x, y, pch = 19, col = 'skyblue')

cor(x,y)
lm(y ~ x)

x <-  runif(n = 300, min = 0, max = 100)
y <-  3* x + 5 + rnorm(100, 0, 20)
plot(x, y, pch = 19, col = 'skyblue')

cor(x,y)
lm(y ~ x)
model <-  lm(y ~ x)
abline(model, col = 'tomato', lwd = 2)

summary(model)

abline(a = 5, b= 5,
       col = 'red',
       lwd = 1,
       lty = 2)

?abline

summary(model)
