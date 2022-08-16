install.packages('gplots')
library(gplots)

df <- data.frame(InsectSprays)

plotmeans(count ~ spray,
          data = df,
          col = 'tomato',
          barcol = 'orange')

# 집단별 상자도표
boxplot(count ~ spray,
        data = df)
aov.result <-  aov(count~spray,
                   data = df)

model.tables(aov.result,
             type = 'mean')

plot(TukeyHSD(aov.result),
     las = 1,
     col = 'tomato')

install.packages('car')
library(car)
qqPlot(df$count,
       pch = 19,
       col = "violet")

shapiro.test(df$count)

oneway.test(count ~ spray, data = df)

# ToothGrowth의 이원분산분석 실행
df <-  ToothGrowth
str(df)
unique(df$dose)

# dose 커럶을 factor 컬럼으로 변경
df$dose <- factor(df$dose,
                  levels = c(0.5, 1, 2),
                  labels = c('L', 'M', 'H')
)
str(df)                  

with(df, tapply(len, INDEX = list(SUPP = supp, DPSE = dose), mean))

# 여러개의 독립변수 사용
boxplot(len ~ supp * dose, data = df,
        col = c('orange', 'tomato'))

aov.result <-  aov(len ~ supp * dose, data = df)
summary(aov.result)        

TukeyHSD(aov.result)
plot(TukeyHSD(aov.result), las = 1)


qnorm(0.95, mean = 50, sd = 10)

plot(mean = 50, sd = 10)

x <- seq(50-3*10, 50+3*10, length.out = 200)
y <- dnorm(x, 50, 10)
plot(x, y, type = 'l', col = "lightgrey", lwd = 2)

