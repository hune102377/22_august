str(sleep)

install.packages('tidyr')
library(tidyr)
wide.df <- spread(sleep, key = group, value = extra)
summary(wide.df)

tapply(sleep$extra,
       INDEX = list(sleep$group),
       FUN = mean)

t.test(extra ~ group, data = sleep, paired = T)
t.test(wide.df$"1", wide.df$"2", paired = T)
