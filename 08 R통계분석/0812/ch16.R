df <-  split(iris, f= iris$Species)
df <-  rbind(df$setosa, df$versicolor)

boxplot(df[, c(1,5)])


# robust 패키지의 breslow.dat 데이터셋 : 뇌전증 환자의 투약 전/후 9주간 발작 횟수
install.packages('robust')
library(robust)
data(breslow.dat)
str(breslow.dat)

df <- breslow.dat
str(df)

df <- df[,c('Base', 'Age', 'Trt', 'sumY')]
str(df)

model <-glm(sumY ~., data = df, family = poisson)
summary(model)

df <- split(iris, f = iris$Species)
df <- rbind(df$setosa, df$versicolor)
plot(df[,c(3,5)])

model <- glm(Species ~ Petal.Length, data = df,
    family = binomial(in))

glm(Species ~ Petal.Lenght, data = d)