### **데이터 불러오기**
data.spring <- read.csv('./Data/LCK_2022_Spring.csv')

data.summer <- read.csv('./Data/LCK_2022_Summer.csv')
data.ranking <- c('T1', 'GEN', 'DK', 'DRX', 'KDF', 'BRO', 'KT', 'NS', 'LSB', "HLE")
data.col <- c('#e4002b', '#aa8a00', '#0ec7B5', '#e73312', '#00492b', '#ff0a07', '#de2027', '#eccf47', '#eccf47', "#ff6b01")

### **데이터 확인**
##### 위에서 10개까지 데이터 보기
head(data.spring, n = 10)

##### 데이터 살펴보기
str(data.spring)

##### 스프링 결측치 확인
sum(is.na(data.spring))
##### 컬럼별 결측치 확인
colSums(is.na(data.spring))
##### 결측값 확인인

##### 94개의 관측값과 13개의 변수들로 구성되어있다
##### 데이터들이 원하는 type이 아니니까 데이터 type을 설정하자

### **데이터 정리**
##### '경기.분류' 데이터 정리
data.summer$경기.분류 <- factor(data.summer$경기.분류)

##### '날짜' 데이터 정리
data.summer$날짜 <-as.Date(data.summer$날짜, format = "%m/%d")
data.summer$승리.예측이.높은.팀 <- as.factor(data.summer$승리.예측이.높은.팀)
data.summer$승리팀 <- as.factor(data.summer$승리팀)
data.summer$예측.성공.여부 <- as.factor(data.summer$예측.성공.여부)

data.summer <- na.omit(data.summer)

##### 여름 결측치 확인
sum(is.na(data.summer))
##### 컬럼별 결측치 확인
colSums(is.na(data.summer))
##### 결측값 확인인
str(data.summer)

##### 94개의 관측값과 13개의 변수들로 구성되어있다
##### 데이터들이 원하는 type이 아니니까 데이터 type을 설정하자

### **데이터 정리**
##### '경기.분류' 데이터 정리
data.spring$경기.분류 <- factor(data.spring$경기.분류)

##### '날짜' 데이터 정리
data.spring$날짜 <-as.Date(data.spring$날짜, format = "%m/%d")
data.spring$승리.예측이.높은.팀 <- as.factor(data.spring$승리.예측이.높은.팀)
data.spring$승리팀 <- as.factor(data.spring$승리팀)
data.spring$예측.성공.여부 <- as.factor(data.spring$예측.성공.여부)

spring <- prop.table(table(data.spring$예측.성공.여부))
summer <- prop.table(table(data.summer$예측.성공.여부))
total <-  spring + summer
total

dt_01 <- cbind(spring,summer)
dt_01

barplot(dt_01)

plot(table(data.spring$예측.성공.여부),
     col = adjustcolor('cyan', alpha = 0.5))
par(new=TRUE)
plot(table(data.summer$예측.성공.여부),
     col = adjustcolor('tomato', alpha = 0.5))

table(data.spring$예측.성공.여부)

dt_temp <-as.factor(data.spring$승리.예측이.높은.팀)

?as.factor

str(dt_temp)
data.spring

##### '팀.1' 데이터 정리
table(data.spring$팀.1)
### **기본 데이터 확인**

#### **경기 분류 확인**

?ifesl

table(data.spring$예측.성공.여부, by = ifelse(data.spring$승리팀 == 1, data.spring$팀.1, data.spring$팀.2))


plot(data.spring)

ifelse(data.spring$승리팀 == 1, data.spring$팀.1, data.spring$팀.2)

table(ifelse(data.spring$승리.예측이.높은.팀 == 1, data.spring$팀.1, data.spring$팀.2))
table(ifelse(data.spring$승리팀 == 1, data.spring$팀.1, data.spring$팀.2))

data.spring$승리팀

plot(x,y)

a <-table(ifelse(data.spring$승리.예측이.높은.팀 == 1, data.spring$팀.1, data.spring$팀.2))
typ

plot(table(ifelse(data.spring$승리.예측이.높은.팀 == 1, data.spring$팀.1, data.spring$팀.2)),
     col = adjustcolor('tomato',  alpha = 0.8),
     ylim = c(0, 20),
     ylab = ''
)
par(new=TRUE)
plot(table(ifelse(data.spring$승리팀 == 1, data.spring$팀.1, data.spring$팀.2)),
     col = adjustcolor('cyan',  alpha = 0.5),
     ylim = c(0, 20),
     ylab = ''
     )

unique(data.spring$팀.1)

barplot(cbind(data.spring$승리.예측이.높은.팀, data.spring$승리팀),
        ~ data.spring.ranking,
        data.spring,
        beside=T
)

### 
plot(table(ifelse(data.spring$승리팀 == 1, data.spring$팀.1, data.spring$팀.2)),
     col = adjustcolor('cyan',  alpha = 0.5),
     ylim = c(0, 20),
     ylab = ''
)
          
