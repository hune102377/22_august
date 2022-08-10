
windows(width = 7, height =5)

v <-  rbinom(n = 100000, size = 1000, prob = 0.4)

hist(v,
     col = 'tomato',
     breaks = 30)

r <- runif(n = 1000000, min = 0, max = 100)

hist(r,
     col = 'cyan',
     breaks = 20)

mean(r)
sd(r)

n <- rnorm(n = 1000, mean = 50, sd = 20)

hist(n,
     col = 'violet',
     breaks = 20)

x <- seq(0, 100, length = 600)
y <-  dnorm(x, mean = 50, sd =  20)
plot(x, y,
     col = 'green',
     lwd = 3,
     type ='l')

x <- seq(0, 100, length = 600)
y <-  dunif(x, min = 0, max =  100)
plot(x, y,
     col = 'green',
     lwd = 3,
     type ='l')

# 연습문제
# 국민소득이 평균이 30000 달러, 표준편차가 10000 달러인 정규분포를 따른다고 가정한다.
# 즉, X를 개인의 소득을 나타내는 확률변수라 할 때,
# X~N(30000,100000^2)
# 어떤 사람의 소득이 25000달러에서 35000 달러 사이에 있을 확률을 구하시오
a <-  pnorm(35000, mean = 30000, sd = 10000)
b <- pnorm(25000, mean = 30000, sd = 10000)
a - b

pnorm(1) -pnorm(-1)
pnorm(2) -pnorm(-2)
pnorm(2.56) -pnorm(-2.56)

(1 - pnorm(87, mean = 68, sd = 10)) * 200

pnorm(87, mean = 68, sd = 10, lower.tail = F)


pnorm(70, 60, 10, lower.tail = F)
pnorm(80, 70, 20, lower.tail = F)


# 동전의 앞면이 나올 확률이 0.5 일떄 동전 던지기를 10000회 실행 그랲
x <-  rbinom(10000, size = 100, prob = 0.5)
hist(x)
curve(dnorm(x, 50, 5), 25, 75)
