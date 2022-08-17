# 카이제곱분석을 통한 장애유형별 정부 지원 만족도 분석

# 독립변수 : 장애 유형
# 종속변수 : 만족도
# 데이터 출처 : https://kosis.kr/statHtml/statHtml.do?orgId=117&tblId=DT_11732S0131&conn_path=I3

# 자료 불러오기
data <- read.csv('장애인_지원_만족도.csv')

# EDA
# 1) str 변수 파악
str(data)

# 설문조사는 매우 많음 / 약간 받고 있음 / 별로 받지 못하고 있음 / 전혀 받지 못하고 있음 으로 나뉘어 있고
# 각 유형별 비율로 이미 나누어 계산되어있음

unique(data$장애유형별)
# 장애 유형의 경우 전체를 제외하면 15개의 소분류로 구분되어 있으며
# https://wis.seoul.go.kr/handicap/policy.do를 기준으로 다시 대분류와 중분류로 구분한다.

# 장애유형의 분류 설정
type.b.e <- c('지체장애', '뇌병변장애', '시각장애', '청각장애', '언어장애', '안면장애', '뇌전증(간질)장애')
type.b.i <- c('신장장애', '심장장애', '간장애', '호흡기장애','장루‧요루장애')
type.m.d <- c('지적장애', '자폐성장애')


# 장애유형의 중분류 설정
data$중분류 <- ifelse(data$장애유형별 %in% type.b.e, '신체외부.장애',
                   ifelse(data$장애유형별 %in% type.b.i, '신체내부.장애',
                          ifelse(data$장애유형별 %in% type.m.d, '발달.장애',
                                 ifelse(data$장애유형별 == '정신장애', '정신장애', '총'))))

# 장애유형의 대분류 설정
data$대분류 <- ifelse(data$장애유형별 %in% type.b.e | data$장애유형별 %in% type.b.i, '신체.장애',
                   ifelse(data$장애유형별 == '총', '총', '정신.장애'))

data      
                   
###
# 자료가 비중이라 카이제곱 검정이 불가능함...
# 전국의 장애인 유형 별 인구 비중으로 표본을 추정하자

# 
data_num <- read.csv('장애인_유형별_인구.csv')
data_all <- merge(data_num, data, by = '장애유형별')

data_all$매우.많음 <- round(data_all$매우.많음 * data_all$계, 0)
data_all$약간.받고.있음 <- round(data_all$약간.받고.있음 * data_all$계, 0)
data_all$별로.받지.못하고.있음 <- round(data_all$별로.받지.못하고.있음 * data_all$계, 0)
data_all$전혀.받지.못하고.있음 <- round(data_all$전혀.받지.못하고.있음 * data_all$계, 0)

data_all <- data_all[order(data_all$중분류),]
data_all <- data_all[order(data_all$대분류),]
rownames(data_all)=NULL
data_all


# 카이제곱 검정
# 범주형 변수 간의 관련성을 확인

# 장애 유형과 만족도 간의 교차표 생성
data.01 <- data_all[-16,]
data.01

'신체.장애' = c(mean(data.01[data.01$대분류 == '신체.장애',3]),
  mean(data.01[data.01$대분류 == '신체.장애',4]),
  mean(data.01[data.01$대분류 == '신체.장애',5]),
  mean(data.01[data.01$대분류 == '신체.장애',6]))

'정신.장애' =c(mean(data.01[data.01$대분류 != '신체.장애',3]),
  mean(data.01[data.01$대분류 != '신체.장애',4]),
  mean(data.01[data.01$대분류 != '신체.장애',5]),
  mean(data.01[data.01$대분류 != '신체.장애',6]))

data_by_big = matrix(c(신체.장애,정신.장애), ncol = 2)
dimnames(data_by_big) <-list("만족도" = c('매우.많음','약간.받고.있음','별로.받지.못하고.있음','전혀.받지.못하고.있음'),
                             "장애유형" = c('신체적.장애', '정신적.장애'))

data_by_big

# 교차표의 행과 열의 합을 추가
addmargins(data_by_big)

# 열이 100% 척도 조정된 열의 비율 계산
addmargins(prop.table(addmargins(data_by_big, 2), 2), 1)

