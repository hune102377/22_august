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

install.packages('palmerpenguins')
library(palmerpenguins)
pg <- penguins
str(pg)
pg <-  pg[complete.cases(pg), -8]
str(pg)
dim(pg)

pg$is.adelie <-  factor(ifelse(pg$species == 'Chinstrap', 'Yes', 'No'))
barplot(table(pg$is.adelie))

pg <- pg[,-1]

model <-  glm(is.adelie ~., data = pg,
               family = binomial(link='logit'))
summary(model)

model$fitted
pg$pred <- ifelse(model$fitted.values > 0.5, 'Yes','No')
table(pg$is.adelie, pg$pred)

df <- iris
df$Species <- factor(ifelse(df$Species == 'virginica', 'Yes', 'No'))

model <-  glm(Species~., data = df,
              family = binomial(link='logit'))
summary(model)

df$pred <-  factor(ifelse(model$fitted.values > 0.5,
                   'Yes', 'No'))
tab <- table(df$Species, df$pred)
TP <- tab[2,2]
TN <-  tab[1,1]
FP <-  tab[2,1]
FN <-  tab[1,2]

accuracy <-  (TP + TN) /(TP + TN + FP + FN)
accuracy
#precision <- 
#recall <- 

install.packages('pROC')
library(pROC)
roc(Species ~ model$fitted.values, data =df,
    plot = T, main = 'Roc curve')
