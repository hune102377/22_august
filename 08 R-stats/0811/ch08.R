v <- rchisq(n = 10000, df = 1)
hist(v, col = 'tomato')

x <-  seq(0, 15, length = 200)
curve(dchisq(x, df = 1), 0, 15,
      col  = 'red',
      lwd = 2,
      lty = 1,)

curve(dchisq(x, df = 5), 0, 15,
      col  = 'cyan',
      lwd = 2,
      lty = 1,
      add = T)

curve(dchisq(x, df = 10), 0, 15,
      col  = 'gold',
      lwd = 2,
      lty = 1,
      add = T)

qchisq(p =0.95, df = 1)
pchisq(q = 2.5, df = 1)
pchisq(q = 3.841459, df = 1)
pchisq(q = 5, df = 1, lower.tail = F)

# 카이스퀘어 검정 따라가기

mt <-  matrix(c(1443, 151, 47, 1781, 312, 135), nrow = 3)
mt

df <-  data.frame(mt)
colnames(df) <- c("With", "Withdout")
rownames(df) <- c("경상", "중상", "사망")
df
str(df)

oij <- c(1443, 1781, 151, 312, 47, 135)
eij <- c(1367, 1855.9, 196.9, 267.4, 77.1, 104.7)
cs.value <- sum((oij -eij)^2 / eij)
cs.value
