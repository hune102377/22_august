### **데이터 불러오기**
#자료 불러오기
data.spring <- read.csv('./Data/LCK_2022_Spring.csv')
data.summer <- read.csv('./Data/LCK_2022_Summer.csv')
data.order <- c('BRO', 'DK', 'DRK', 'GEN', 'HLE', 'KDF', 'KT', 'LSB', 'NS', "T1")
data.col <- c('#00492b', '#0ec7B5', '#2f5ff7', '#aa8a00', '#ff6b01', '#e73312', '#ff0a07', '#eccf47', '#de2027', "#e4002b")

# 결측치 처리하기
data.spring <- na.omit(data.spring)
data.summer <- na.omit(data.summer)

# 각 팀의 평균적인 기대치와 승점 간의 관계

# 평균 기대치 정리

func_pro <- function(x){
  pro.team.spring = sum(data.spring[data.spring$팀.1 == x, 5]) / length(data.spring[data.spring$팀.1 == x, 5]) + sum(data.spring[data.spring$팀.2 == x, 6]) / length(data.spring[data.spring$팀.2 == x, 6]) 
  pro.team.spring = pro.team.spring / 2
  pro.team.spring
  
  pro.team.summer = sum(data.summer[data.summer$팀.1 == x, 5]) / length(data.summer[data.summer$팀.1 == x, 5]) + sum(data.summer[data.summer$팀.2 == x, 6]) / length(data.summer[data.summer$팀.2 == x, 6]) 
  pro.team.summer = pro.team.summer / 2
  pro.team.summer
  
  pro.team = (pro.team.spring + pro.team.summer) / 2
  pro.team
  
  win.team.spring <- 100 * (sum(data.spring[data.spring$팀.1 == x, '승리팀'] == 1) + sum(data.spring[data.spring$팀.2 == x, '승리팀'] == 2)) / (length(data.spring[data.spring$팀.1 == x, 1]) + length(data.spring[data.spring$팀.2 == x, 1]))
  win.team.summer <- 100 * (sum(data.summer[data.summer$팀.1 == x, '승리팀'] == 1) + sum(data.summer[data.summer$팀.2 == x, '승리팀'] == 2)) / (length(data.summer[data.summer$팀.1 == x, 1]) + length(data.summer[data.summer$팀.2 == x, 1]))
  win.team = (win.team.spring + win.team.summer) / 2
  
  print(x)
  cat("스프링 기대치 : ", pro.team.spring, '\n')
  cat("써머 기대치 : ", pro.team.summer, '\n')
  cat("전체 기대치 : ", pro.team, '\n')
  
  cat("스프링 승률 : ", win.team.spring, '\n')
  cat("써머 승률 : ", win.team.summer, '\n')
  cat("전체 승률 : ", win.team, '\n')
}
func_pro('BRO')
# 문제는 이걸 한번에 정리하는 함수로 만들어야됨됨


