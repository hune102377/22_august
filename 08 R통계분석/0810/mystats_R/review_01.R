library(MASS)
str(survey) # student survey data
# 237 obs, 12 variables, data.frame

levels(survey$W.Hnd) # survey 데이터의 Writing Hand의 값

table(survey$W.Hnd) # Writing Hand 분석값의 빈도값

# Writing Hand 분석값의 빈도값 시각화
barplot(table(survey$W.Hnd),
        col = 'steelBlue',
        xlab = "Writing Hand",
        ylab = "Frequency",
        main = "Frequency table of writing Hand")

prop.table(table(survey$W.Hnd)) # Writing Hand 분석값의 빈도의 비율

# 연속형 변수의 데이터 탐색
survey$Height
# NA(결측치)가 많음

height <-  survey$Height

length(height)

mean(height) # 결측치가 있어서 값이 안나옴

mean(height, na.rm = T) # 결측치를 제거하고 평균

median(height, na.rm = T)

max(height, na.rm = T)

min(height, na.rm = T)

# 백분위 수를 구해주는 quantile 함수
quantile(height, probs = 0.9, na.rm = T)

quantile(height, probs = c(0.25, 0.75), na.rm =T)

# heigt 자료의 도수분포표
hist(height,
     col = 'steelblue',
     breaks = 15,
     xlim = c(140, 210),
     ylim = c(0, 50)
     )

# 데이터의 요약을 시각적으로 표시해주는 stargazer 패키지
#install.packages('stargazer')
library(stargazer)
stargazer(survey,
          type = 'text',
          title = "Summary of survey dataset")

#  Arthritis 데이터 셋 분석
# install.packages("vcd")
library(vcd)
str(Arthritis)
# 84개의 관측값과 5개의 컬럼으로 구성된 데이터프레임

#교차표
table(Arthritis$Improved, Arthritis$Treatment)

with(Arthritis, table(Improved, Treatment))

xtabs(~ Improved + Treatment, data = Arthritis)

# 교차표 시각화
mosaic(Improved ~ Treatment, data = Arthritis, gp = shading_max)

# r 교차표의 행과 열을 기준으로 빈도수의 합과 비율을 계산
with(Arthritis, table(Improved, Treatment))

cross.tab <-  with(Arthritis, table(Improved, Treatment))
margin.table(cross.tab, margin = 1) # 행을 기준으로

margin.table(cross.tab, margin = 2) # 열을 기준으로

prop.table(cross.tab, margin = 1) # 행을 기준으로

prop.table(cross.tab, margin = 2) # 열을 기준으로

# 테이블의 합을 구하는 addmargins 함수
addmargins(cross.tab)

addmargins(cross.tab, margin = 1)

# gmodels 패키지의 CrossTable() 함수 : 교차분석을 위한 다양한 정보를 담은 교차표 생성
# install.packages("gmodels")
library(gmodels)
with(Arthritis, CrossTable(Improved, Treatment,
                           prop.r = F,
                           prop.c = F,
                           prop.t = T,
                           prop.chisq = F ))

# 다차원 테이블 : 세개의 범주형 변수 간의 관계를 파악
cross.tab <-  with(Arthritis, table(Improved, Sex, Treatment))
ftable(cross.tab) # 평면분할표
ftable(cross.tab, row.vars = c(2,3))

ftable(prop.table(cross.tab, margin =c(2,3)))
