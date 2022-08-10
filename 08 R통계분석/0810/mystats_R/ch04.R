
library(MASS)
height <- na.omit(survey$Height)
length(height)
hist(height,
     col = 'skyblue',
     breaks = 20)

X.bar <- c()
for (i in 1:100000) {
  samp <- height[sample(1:209, size = 30)]
  X.bar[i] <- mean(samp)
  X.sd[i] <- sd(samp)
}
hist(X.bar,
     col = 'cyan',
     breaks = 20, prob = T)
x <-  seq(160, 180, length = 200)
curve(dnorm(x, mean(height), sd(X.bar)),
      160, 180, col = 'tomato',
      add = T, lwd = 3, lty =2)

x.1 <- rnorm(n = 5000, mean = 70, sd = 5)
x.2 <- rnorm(n = 5000, mean = 50, sd = 5)
x <- c(x.1, x.2)
hist(x, col = 'skyblue', breaks = 20)

X.bar <- c()
for (i in 1:100000) {
  samp <- x[sample(x, size = 30)]
  X.bar[i] <- mean(samp)
}
hist(X.bar,
     col = 'cyan',
     breaks = 20, prob = T)

num_iris <- iris[,-5]

cor(num_iris)

cor.test(num_iris$Petal.Length, num_iris$Petal.Width)
