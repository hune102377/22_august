# 이항분포와 가설검정
# • 100번의 동전을 던져서 앞면이 60번 나왔다면, 공평한 동전이라고 할 수 있는가?
# 𝐻0: 이 동전은 공평한 동전과 다르지 않다(성공확률이 𝑝 = 0.5이다).
# 𝐻1: 이 동전은 공평한 동전과 다르다(성공확률이 𝑝 ≠ 0.5이다). 

binom.test(x = 60, n = 100, p = 0.5)

# 귀무가설을 기각할 수 없다.
# p-value가 0,05보다 크니 신뢰구간 95%에 포함된다.

# 정규분포와 분위수함수
qnorm(p = 0.5, mean =50, sd =10)
pnorm(50, mean = 50, sd = 10)


qnorm(p = 0.975, mean =50, sd =10)
pnorm(75.75829, mean = 50, sd = 10)


qnorm(p =0.024, mean = 50, sd =10)
pnorm(30.22632, mean = 50, sd = 10)


qnorm(p = 0.005, mean = 50, sd = 10)
pnorm(24.24171, mean = 50, sd = 10)


qnorm(p = 0.995, mean = 50, sd = 10)
pnorm(75.75829, mean = 50, sd = 10)


# 이항분포와 가설검정
# • 100번의 동전을 던져서 앞면이 65번 나왔다면, 공평한 동전이라고 할 수 있는가?
# 𝐻0: 이 동전은 공평한 동전과 다르지 않다(성공확률이 𝑝 = 0.5이다).
# 𝐻1: 이 동전은 공평한 동전과 다르다(성공확률이 𝑝 ≠ 0.5이다).


binom.test(x = 65, n = 100, p = 0.5)
# 귀무가설을 기각할 수 있다.
# p-value가 0.5보다 작으니 신뢰구간 95%에 포함되지 않는다.

# 이항분포와 가설검정
# • 100번의 동전을 던져서 앞면이 35번 나왔다면, 공평한 동전이라고 할 수 있는가?
# 𝐻0: 이 동전은 공평한 동전과 다르지 않다(성공확률이 𝑝 = 0.5이다).
# 𝐻1: 이 동전은 공평한 동전과 다르다(성공확률이 𝑝 ≠ 0.5이다).
# 단, 유의수준은 99%
binom.test(x = 35, n = 100, p = 0.5, conf.level = 0.99)

# 정규성 검정
# shaprio-wilk
library(MASS)
shapiro.test(survey$Height)
hist(survey$Height)

shapiro.test(survey$Age)
shapiro.test(iris$Petal.Length)

shapiro.test(mtcars$mpg)

v <-  rt(n = 10000, df = 29)
hist(v,col='skyblue', prob=1)

x <-seq(-4, 4, length = 200)
curve(dt(x, df = 29),
      -4,4,
      col='tomato', 
      lty=2,lwd=2, 
      add = T)
curve(dnorm(x), -4,4, add=T, col = 'violet', lwd =5, lty=4)