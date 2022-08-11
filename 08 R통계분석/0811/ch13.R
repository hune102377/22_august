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
lm(childHeight ~ midparentHeight, data = df)
