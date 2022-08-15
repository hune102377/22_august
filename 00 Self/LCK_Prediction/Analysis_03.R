data.summer <- read.csv('./Data/LCK_2022_Summer.csv')
data.col <- c('#00492b', '#0ec7B5', '#2f5ff7', '#aa8a00', '#ff6b01', '#e73312', '#ff0a07', '#eccf47', '#de2027', '#e4002b')
              
data.summer$날짜 <-as.Date(data.summer$날짜, format = "%m/%d")
data.summer$승리.예측이.높은.팀 <- as.factor(data.summer$승리.예측이.높은.팀)
data.summer$승리팀 <- as.factor(data.summer$승리팀)
data.summer$예측.성공.여부 <- as.factor(data.summer$예측.성공.여부)

data.summer <- na.omit(data.summer)

num.pre.data <- table(ifelse(data.summer$승리.예측이.높은.팀 == 1, data.summer$팀.1, data.summer$팀.2))
num.win.data <- table(ifelse(data.summer$승리팀 == 1, data.summer$팀.1, data.summer$팀.2))

num.pre.data$'HLE' <- as.integer('0')
num.pre.data

num.pre.data <- num.pre.data[c(1,2,3,4,10,5,6,7,8,9)]
total.data.summer <- rbind(num.pre.data, num.win.data)
str(total.data.summer)
total.data.summer <- data.frame(t(total.data.summer))
class(total.data.summer)
total.data.summer
str(total.data.summer)
barplot(total.data.summer$num.pre.data,
     col = adjustcolor(data.col, alpha = 0.5))
