library(car)
data(Prestige)
df <- Prestige
str(df)

table(df$type)
barplot(table(df$type),
        col = 'orange')

hist(df$income,
     col = 'tomato',
     breaks = 20)

shapiro.test(df$income)

hist(df$education,
     col = 'pink',
     breaks = 20)

hist(df$women,
     col = 'violet',
     breaks = 20)

hist(df$prestige,
     col = 'cyan',
     breaks = 20)

shapiro.test(df$prestige)

plot(df[, -(5:6)],
     pch = '+',
     col = 'steelblue')

lm(income ~ education, data = df)

cor(df[,-(5:6)])

model <-lm(income ~ education, data = df)
summary(model)

plot(income ~ education, data = df,
     xlim = c(0, 16),
     ylim = c(-5000, 25000),
     col = 'skyblue',
     pch = '+')
abline(model,
       col = adjustcolor('tomato', alpha = 0.8))

model_02 <-lm(income ~ women, data = df)
summary(model_02)

plot(income ~ women, data = df,
     col = 'violet',
     pch = '+')
abline(model_02,
       col = adjustcolor('tomato', alpha = 0.8))

model_03 <-lm(income ~ prestige, data = df)
summary(model_03)

plot(income ~ prestige, data = df,
     col = 'tomato',
     xlim = c(0, 100),
     ylim = c(-5000, 25000),
     pch = '+')
abline(model_03,
       col = adjustcolor('tomato', alpha = 0.8))

# 다중회귀분선
model <-  lm(income ~., data = df)
summary(model)

# 교육과 임금과의 상관관계가 없어졌다
# 교육과 명성 사이의 관계를 고려했기 때문

model <-  lm(income ~ education + women, data = df)
summary(model)

model <-  lm(income ~ education + prestige, data = df)
summary(model)

model <-  lm(income ~ women + prestige, data = df)
summary(model)


library(stargazer)
stargazer(model, type = 'text')

par(mfrow = c(2,2))
plot(model)
par(mflow = c(1,1))

model <-  lm(income ~ education , data = df)
plot(income ~ education, data = df,
     col = 'skyblue',
     pch = 16)

model <- lm(income ~ education + I(education^2),
data =df )
summary(model)

library(dplyr)
model <- lm(income ~ education + I(education^2),
            data =df )
plot(income ~ education, data = df,
     col = 'skyblue', pch ='+')
with(df,
     lines(arrange(data.frame(education, fitted(model)),
                   education),
           lty = 1, lwd = 3, col ='tomato'))