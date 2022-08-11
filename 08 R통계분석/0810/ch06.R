library(MASS)
data(cats)
str(cats)

t.test(Bwt ~ Sex, data=cats)
t.test(Bwt ~ Sex, data=cats, conf.level =0.99)
meand(cats$Bwt)
tapply(cats$Bwt, INDEX = list(Sex = cats$Sex), mean)

t.test(Hwt ~ Sex, data=cats)

t.test(Hwt ~ Sex, data=cats, conf.levle = 0.99)

str(sleep)

t.test(extra ~ group, data = sleep, paired =T)

hist(sleep$extra)
?sleep


sleep1 <- with(sleep, extra[group == 2] - extra[group == 1])
summary(sleep1)
stripchart(sleep1, method = "stack", xlab = "hours",
           main = "Sleep prolongation (n = 10)")
boxplot(sleep1, horizontal = TRUE, add = TRUE,
        at = .6, pars = list(boxwex = 0.5, staplewex = 0.25))
