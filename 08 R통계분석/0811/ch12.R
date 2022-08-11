cor(cats$Bwt, cats$Hwt)
plot(cats$Bwt, cats$Hwt,
     pch = 19,
     col = 'tomato')

# 상관 분석에서 기본적으로 사용하는 상관계수
cor(cats$Bwt, cats$Hwt,
     method = "pearson")

# 순위(rank) 데이터를 바탕으로 계산하므로 이상점에 덜 민감
cor(cats$Bwt, cats$Hwt,
    method = "spearman")

# 샘플 사이즈가 작거나 데이터의 동률이 많을 때 유용함
cor(cats$Bwt, cats$Hwt,
    method = "kendall")

