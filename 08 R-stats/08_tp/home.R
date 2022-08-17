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
# https://kosis.kr/statHtml/statHtml.do?orgId=117&tblId=DT_11732S0132&vw_cd=MT_ZTITLE&list_id=117_11732_10_002_10&scrId=&seqNo=&lang_mode=ko&obj_var_id=&itm_id=&conn_path=MT_ZTITLE&path=%252FstatisticsList%252FstatisticsListIndex.do

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

##

# 카이제곱 검정
# 범주형 변수 간의 관련성을 확인

#####

# 대분류장애 유형과 만족도 간의 교차표 생성
data.01 <- data_all[-16,]
data.01

data.01

'신체.장애' = c(sum(data.01[data.01$대분류 == '신체.장애',3]),
            sum(data.01[data.01$대분류 == '신체.장애',4]),
            sum(data.01[data.01$대분류 == '신체.장애',5]),
            sum(data.01[data.01$대분류 == '신체.장애',6]))

'정신.장애' =c(sum(data.01[data.01$대분류 != '신체.장애',3]),
           sum(data.01[data.01$대분류 != '신체.장애',4]),
           sum(data.01[data.01$대분류 != '신체.장애',5]),
           sum(data.01[data.01$대분류 != '신체.장애',6]))

data_by_big = matrix(c(신체.장애,정신.장애), ncol = 2)
dimnames(data_by_big) <-list("만족도" = c('매우.많음','약간.받고.있음','별로.받지.못하고.있음','전혀.받지.못하고.있음'),
                             "장애유형" = c('신체적.장애', '정신적.장애'))

data_by_big

# 교차표의 행과 열의 합을 추가
addmargins(data_by_big)

# 열이 100% 척도 조정된 열의 비율 계산
addmargins(prop.table(addmargins(data_by_big, 2), 2), 1)

# 총 249566380개의 응답에서 신체적 장애와 관련된 응답은 212761426개이고, 정신적 장애와 관련된 응답은 36804954개이다.
# 신체적 장애 응답 중 부정적인 답변('별로.받지.못하고.있음'과 '전혀.받지.못하고.있음')은 143184141(110382080 + 32802061)개로
# 비율로는 약 67%이다.
# 정신적 장애 응답 중 부정적인 답변은 15408374개이고 약 42%이다.
# 신체적 장애의 부정적인 응답의 비중이 정신적 장애의 부정적인 응답의 비중보다 높다.
# 반면에 '매우.많음'의 경우에는 신체적 장애가 약 3%, 정신적 장애가 약 7%로 두 배 이상 긍정적인 답변한 사실을 알 수 있다.

# 이를 그래프로 시각화하면 다음과 같다
data_by_big.prop <- prop.table(data_by_big,2)
barplot(data_by_big.prop * 100, las =1,
        legend = T,
        col = c('blue', 'cyan', 'tomato', 'red'),
        ylab = '만족도',
        main = '대분류에 따른 정부 보조 만족도')

# 시각적으로 정신적 장애의 경우 정부 보조에 대한 만족도가 높은 것을 알 수 있다.

# 카이제곱값 구하기
# '매우.많음'의 비율 : 약 4%
# '약간.받고.있음'의 비율 : 약 33%
# '별로.받지.못하고.있음'의 비율 : 약 49%
# '전혀.받지.못하고.있음'의 비율 : 약 14%

# 신체적 장애의 '매우.많음' 기대빈도
a01 <- 4 * 212761426
# 정신적 장애의 '매우.많음' 기대빈도
a02 <- 4 * 36804954 

# 신체적 장애의 '약간.받고.있음' 기대빈도
b01 <- 33 * 212761426
# 정신적 장애의 '약간.받고.있음' 기대빈도
b02 <- 33 * 36804954 

# 신체적 장애의 '별로.받지.못하고.있음 ' 기대빈도
c01 <- 49 * 212761426
# 정신적 장애의 '별로.받지.못하고.있음 ' 기대빈도
c02 <- 49 * 36804954 

# 신체적 장애의 '전혀.받지.못하고.있음' 기대빈도
d01 <- 14 * 212761426
# 정신적 장애의 '전혀.받지.못하고.있음' 기대빈도
d02 <- 14 * 36804954 

data_by_big[2,1]

kai <- ((((data_by_big[1,1]-a01)^2) / a01) +
          ((data_by_big[1,2] - a02)^2 / a02) +
          ((data_by_big[2,1] - b01)^2 / b01) +
          ((data_by_big[2,2] - b02)^2 / b02) +
          ((data_by_big[3,1] - c01)^2 / c01) +
          ((data_by_big[3,2] - c02)^2 / c02) +
          ((data_by_big[4,1] - d01)^2 / d01) +
          ((data_by_big[4,2] - d02)^2 / d02))

# pchisq() 함수를 활용하여 유의확률 확인
pchisq(kai, df = 2, lower.tail=F)

# 유의확률이 0이므로 귀무가설
# '장애의 대분류에 따른 만족도 간의 관계가 없다'을 기각할 수 있다.

# 독립성검정
# chisq.text() 함수를 이용하여 독립성검정을 수행
chisq.test(data_by_big)

# 검정 결과 p-값이 유의수준 0.05에 비해서 매우 작으므로 대분류에 따른 정부 보조 만족도과 관계까 없다는 귀무가설을 기갈할 수 있따.

# vcd 패키지의 assocstats() 함수로 관련성의 강도를 측정하는 지표를 계산
# 이 지표들은 그 값이 클수록 두 변수 간의 관련성이 크다는 것을 의미
#install.packages('vcd')
library(vcd)
assocstats(data_by_big)

# 범주형 변수 간의 관계를 모자이크도표를 이용하여 시각화
mosaicplot(t(data_by_big),
       main = '대분류에 따른 정부 보조 만족도',
       col = c('blue', 'cyan', 'tomato', 'red'))

#######??? 그냥 앞에서 한 카이 검정이랑 다를게 뭐임?

#####


# 대분류장애 유형과 만족도 간의 교차표 생성
data.01 <- data_all[-16,]
data.01

'신체내부.장애' = c(sum(data.01[data.01$중분류 == '신체내부.장애',3]),
            sum(data.01[data.01$중분류 == '신체내부.장애',4]),
            sum(data.01[data.01$중분류 == '신체내부.장애',5]),
            sum(data.01[data.01$중분류 == '신체내부.장애',6]))

'신체외부.장애' = c(sum(data.01[data.01$중분류 == '신체외부.장애',3]),
              sum(data.01[data.01$중분류 == '신체외부.장애',4]),
              sum(data.01[data.01$중분류 == '신체외부.장애',5]),
              sum(data.01[data.01$중분류 == '신체외부.장애',6]))

'발달.장애' =c(sum(data.01[data.01$중분류 == '발달.장애',3]),
           sum(data.01[data.01$중분류 == '발달.장애',4]),
           sum(data.01[data.01$중분류 == '발달.장애',5]),
           sum(data.01[data.01$중분류 == '발달.장애',6]))

'정신장애' =c(sum(data.01[data.01$중분류 == '정신장애',3]),
           sum(data.01[data.01$중분류 == '정신장애',4]),
           sum(data.01[data.01$중분류 == '정신장애',5]),
           sum(data.01[data.01$중분류 == '정신장애',6]))


data_by_mid = matrix(c(신체내부.장애,신체외부.장애,발달.장애,정신장애), ncol = 4)
dimnames(data_by_mid) <-list("만족도" = c('매우.많음','약간.받고.있음','별로.받지.못하고.있음','전혀.받지.못하고.있음'),
                             "장애유형" = c('신체내부.장애', '신체외부.장애','발달.장애', '정신장애'))
data_by_mid

# 교차표의 행과 열의 합을 추가
addmargins(data_by_mid)

# 열이 100% 척도 조정된 열의 비율 계산
addmargins(prop.table(addmargins(data_by_mid, 2), 2), 1)


# 이를 그래프로 시각화하면 다음과 같다
data_by_mid.prop <- prop.table(data_by_mid,2)
barplot(data_by_mid.prop * 100, las =1,
        col = c('blue', 'cyan', 'tomato', 'red'),
        ylab = '만족도',
        main = '중분류에 따른 정부 보조 만족도')

# 시각적으로 '신체외부.장애'의 비중이 높은 것을 알 수 있다.

# 카이제곱값 구하기
# '매우.많음'의 비율 : 약 4%
# '약간.받고.있음'의 비율 : 약 33%
# '별로.받지.못하고.있음'의 비율 : 약 49%
# '전혀.받지.못하고.있음'의 비율 : 약 14%

# 신체내부.장애의 '매우.많음' 기대빈도
a01 <- 4 * 11289510
# 신체외부.장애의 '매우.많음' 기대빈도
a02 <- 4 * 201471916 
# 발달.장애의 '매우.많음' 기대빈도
a03 <- 4 * 27108653 
# 정신장애의 '매우.많음' 기대빈도
a04 <- 4 * 9696301 

# 신체내부.장애의 '약간.받고.있음' 기대빈도
b01 <- 33 * 11289510
# 신체외부.장애의 '약간.받고.있음' 기대빈도
b02 <- 33 * 201471916 
# 발달.장애의 '약간.받고.있음' 기대빈도
b03 <- 33 * 27108653 
# 정신장애의 '약간.받고.있음' 기대빈도
b04 <- 33 * 9696301 

# 신체내부.장애의 '별로.받지.못하고.있음' 기대빈도
c01 <- 49 * 11289510
# 신체외부.장애의 '별로.받지.못하고.있음' 기대빈도
c02 <- 49 * 201471916 
# 발달.장애의 '별로.받지.못하고.있음' 기대빈도
c03 <- 49 * 27108653 
# 정신장애의 '별로.받지.못하고.있음' 기대빈도
c04 <- 49 * 9696301 

# 신체내부.장애의 '전혀.받지.못하고.있음' 기대빈도
d01 <- 14 * 11289510
# 신체외부.장애의 '전혀.받지.못하고.있음' 기대빈도
d02 <- 14 * 201471916 
# 발달.장애의 '전혀.받지.못하고.있음' 기대빈도
d03 <- 14 * 27108653 
# 정신장애의 '약간.받고.있음' 기대빈도
d04 <- 14 * 9696301 

kai_02 <- ((((data_by_mid[1,1]-a01)^2) / a01) +
             ((data_by_mid[1,2] - a02)^2 / a02) +
             ((data_by_mid[1,3] - a03)^2 / a03) +
             ((data_by_mid[1,4] - a04)^2 / a04) +
             ((data_by_mid[2,1] - b01)^2 / b01) +
             ((data_by_mid[2,2] - b02)^2 / b02) +
             ((data_by_mid[2,3] - b03)^2 / b03) +
             ((data_by_mid[2,4] - c04)^2 / b04) +
             ((data_by_mid[3,1] - c01)^2 / c01) +
             ((data_by_mid[3,2] - c02)^2 / c02) +
             ((data_by_mid[3,3] - c03)^2 / c03) +
             ((data_by_mid[3,4] - c04)^2 / c04) +
             ((data_by_mid[4,1] - d01)^2 / d01) +
             ((data_by_mid[4,2] - d02)^2 / d02) +
             ((data_by_mid[4,3] - d03)^2 / d03) +
             ((data_by_mid[4,4] - d04)^2 / d04))

# pchisq() 함수를 활용하여 유의확률 확인
pchisq(kai_02, df = 2, lower.tail=F)

# 유의확률이 0이므로 귀무가설
# '장애의 중분류에 따른 만족도 간의 관계가 없다'을 기각할 수 있다.

# 독립성검정
# chisq.text() 함수를 이용하여 독립성검정을 수행
chisq.test(data_by_mid)

# 검정 결과 p-값이 유의수준 0.05에 비해서 매우 작으므로 중분류에 따른 정부 보조 만족도과 관계까 없다는 귀무가설을 기갈할 수 있따.

# vcd 패키지의 assocstats() 함수로 관련성의 강도를 측정하는 지표를 계산
# 이 지표들은 그 값이 클수록 두 변수 간의 관련성이 크다는 것을 의미
assocstats(data_by_mid)

# 범주형 변수 간의 관계를 모자이크도표를 이용하여 시각화
mosaicplot(t(data_by_mid), legend=T,
           main = '중분류에 따른 정부 보조 만족도',
           col = c('blue', 'cyan', 'tomato', 'red'))

#######??? 그냥 앞에서 한 카이 검정이랑 다를게 뭐임?


## '신체.외부.장애' 내의 변수 분석
data.01.b.e <-  data.01[data.01$중분류 == '신체외부.장애',]

data.01.b.e

data.01.b.e = matrix(c(data.01.b.e$매우.많음,data.01.b.e$약간.받고.있음,data.01.b.e$별로.받지.못하고.있음,data.01.b.e$전혀.받지.못하고.있음), ncol = 4)
colnames(data.01.b.e) = c('매우.만족', '약간.받고.있음','별로.받지.못하고.있음','전혀.받지.못하고.있음')
rownames(data.01.b.e) = c('뇌병변장애', '뇌전증(간질)장애', '시각장애','안면장애', '언어장애', '지체장애', '청각장애')

t <- t(data.01.b.e)
# 교차표의 행과 열의 합을 추가
t


a# 이를 그래프로 시각화하면 다음과 같다
t.prop <- prop.table(t,2)
t.prop
barplot((t.prop * 100), las =1,
        col = c('blue', 'cyan', 'tomato', 'red'),
        ylab = '만족도',
        main = '소분류에 따른 정부 보조 만족도')



### 왜 이런 결과가 나올까?

data.03 <-read.csv('장애인_복지_이용.csv')
barplot(data.03$빈도,
        main = '장애인 복지 사업',
        names = data.03$사업유형별,
        cex.lab = 0.4,
        col = c('grey','grey', 'grey', 'grey', 'grey',
                'cyan','cyan','cyan','cyan',
                'grey','grey', 'grey', 'grey', 'grey',
                'grey','grey', 'grey', 'grey', 'grey',
                'grey','grey', 'grey', 'grey', 'grey',
                'grey','grey'
                ))
# 정부 지원은 직접적인 의료 관련 복지보단 교통 요금감면 등에 집중되어있는데
# https://kosis.kr/statHtml/statHtml.do?orgId=117&tblId=DT_11732S0132&conn_path=I3

data.04 <-read.csv('장애인_추가_지출.csv')
data.04 <- data.04[-1, ]
barplot(data.04$추가.지출,
        main = '장애인 추가 지출',
        names = data.04$지출항목별.1.,
        cex.lab = 0.4,
        col = c('grey', 'cyan','grey', 'grey', 'grey', 'grey', 'grey', 'grey', 'grey'))
# 지체 장애인들이 추가적인 지출은 직접적인 의료비 복지임을 알 수 있다.
# https://kosis.kr/statHtml/statHtml.do?orgId=117&tblId=DT_11732S0124&conn_path=I3

## 따라서 '장애인은 장애 유형과 관계없이 복지에 대해 불만족이다.'
## 라는 일반적인 생각과는 달리
## 신체적 장애, 그 중에서도 지체장애의 경우 유의미하게 더 불만족이 심하고
## 이는 지체 장애인이 주로 원하는 지원이 의료비 지원인 반면 실제 지원은
## 미흡하다는 사실 때문임을 알 수 있다.