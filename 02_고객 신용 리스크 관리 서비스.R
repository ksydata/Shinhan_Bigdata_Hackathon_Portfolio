#2. 위험 고객 분석 ####


#2.1. sbh_H 집단 내 연령별 시각화 ####

# 집단 내 연령 수
sbh_H2 <- sbh_H %>% group_by(P2) %>% summarise(n_age=n())

#시각화-막대그래프
library(ggplot2)
ggplot(data=sbh_H2, aes(x=P2, y=n_age))+geom_bar(stat='identity', fill='steelblue')+labs(x="연령", y="빈도")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("위험 등급 내 연령 빈도")


#2.2. 카이제곱 검정 - 위험 수준&연령 간 연관성 파악 ####

xtabs(~ P2 + G1, data=sbh_H)
prop.table(xtabs(~P2+G1, data=sbh_H), margin=1)
chisq.test(xtabs(~P2+G1, data=sbh_H))


#2.3. 이항로지스틱 회귀분석 - 위험 수준 & 산업 연관성 파악 : 선형 대신에 시그모이드 곡선(S자형태의 곡선)을 이용한 회귀 방법 ####

logit1 <- glm(formula = G1 ~ C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24+C25+C26+C27, family = 'binomial', data = sbh_H)
summary(logit1)

# 산점도 및 지터 차트
ggplot(data = sbh_H, aes(x = nE1, y = C5)) + geom_point()
ggplot(data = sbh_H, aes(x = nE1, y = C18)) + geom_point()
ggplot(data = sbh_H, aes(x = nE1, y = C27)) + geom_point()

ggplot(data = sbh_H, aes(x = nE1, y = C5)) + geom_jitter(aes(col = C5), height = 0.1, width = 0.1)
ggplot(data = sbh_H, aes(x = nE1, y = C18)) + geom_jitter(aes(col = C18), height = 0.1, width = 0.1)
ggplot(data = sbh_H, aes(x = nE1, y = C27)) + geom_jitter(aes(col = C27), height = 0.1, width = 0.1)


## 2.3.1. 위 가설검정에서 음의 상관관계 나타내는 산업 HL, L 에서 비교 (Hl, L 이랑 비교할 때는 양의 상관관계를 띄는지 = HL일 확률을 높여주는지) ####

sbh_HL_L <- sbh %>% filter(G1=="HL" | G1 == "L")
sbh_HL_L$G1 <- as.factor(sbh_HL_L$G1)

logit2 <- glm(formula = G1 ~ C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24+C25+C26+C27, family = 'binomial', data = sbh_HL_L)
summary(logit2)

# 산점도 및 지터 차트
ggplot(data = sbh_HL_L, aes(x = nE1, y = C4)) + geom_point()
ggplot(data = sbh_HL_L, aes(x = nE1, y = C16)) + geom_point()

ggplot(data = sbh_H, aes(x = nE1, y = C4)) + geom_jitter(aes(col = C4), height = 0.1, width = 0.1)
ggplot(data = sbh_H, aes(x = nE1, y = C16)) + geom_jitter(aes(col = C16), height = 0.1, width = 0.1)


#2.4. C4, C5, C16, C18, C27 산업 고객 데이터 추출 ####


#2.4.1 sbh_H에서 양의 상관관계 보인 산업 (C5, C18, C27) ####

#sbh_H_C5 <- sbh_H %>% select(c(1:8, B12, B106, B107, C5, G1))

# 2.4.1. (1) C5

quantile(sbh_H$C5, probs=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) #95% 498000
length(which(sbh_H$C5 >= 498000)) # 95%값 이상인 경우 4451개 (상위 5%)
  # 상위 5%만 추출

sbh_H_C5 <- sbh_H %>% filter(C5 >= 498000) %>% select(c(1:8,B12, B106, B107, C1, C5, 209:215))
sbh_H_C5 <- sbh_H_C5[order(sbh_H_C5$C5, decreasing = T),] #C5가 많은 순으로 ordering
table(sbh_H_C5$G1) #HL인 사람 7명, 567  780 1308 2087 2630 2748 3752번

# C5 중 연령별 해당 산업 내 지출 항목 소비 빈도 분석
# a. B12
H_B12 <- sbh_H_C5 %>% group_by(P2) %>% summarise(B12=mean(B12))
ggplot(data = H_B12, aes(x=P2, y=B12))+geom_bar(stat='identity', fill='gold')+labs(x="연령", y="세금_공과금")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 세금_공과금 금액 비교")
  # 꺾은 선 그래프
ggplot(data = H_B12, aes(x=P2, y=B12, group=1))+geom_line(size=2, color='gold')+geom_point(size=4)+labs(x="연령", y="세금_공과금")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 세금_공과금 금액 비교")
H_B12_50 <- sbh_H_C5 %>% filter(P2 == "50대_초") %>% select(c(1:8, B12, C1, C5, 14:20))
  # B12 상위 5% 소비 고객 중 50대 초반 연령 추출


# b. B106
H_B106 <- sbh_H_C5 %>% group_by(P2) %>% summarise(B106=mean(B106))
ggplot(data = H_B106, aes(x=P2, y=B106))+geom_bar(stat='identity', fill='gold')+labs(x="연령", y="생명보험")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 생명보험 금액 비교")
  # 꺾은 선 그래프
ggplot(data = H_B106, aes(x=P2, y=B106, group=1))+geom_line(size=2, color='gold')+geom_point(size=4)+labs(x="연령", y="생명보험")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 생명보험 금액 비교")

# c. B107
H_B107 <- sbh_H_C5 %>% group_by(P2) %>% summarise(B107=mean(B107))
ggplot(data = H_B107, aes(x=P2, y=B107))+geom_bar(stat='identity', fill='gold')+labs(x="연령", y="손해보험")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 손해보험 금액 비교")
  # 꺾은 선 그래프
ggplot(data = H_B107, aes(x=P2, y=B107, group=1))+geom_line(size=2, color='gold')+geom_point(size=4)+labs(x="연령", y="손해보험")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 손해보험 금액 비교")


# 2.4.1. (2) C18

quantile(sbh_H$C18, probs=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) 
  # 다 0
length(which(sbh_H$C18 != 0)) #0이 아닌 데이터 2896개
  # 일단 0이 아닌 데이터만 추출
sbh_H_C18 <- sbh_H %>% filter(C18 != 0) %>% select(c(1:8, B74, B75, B76, C1, C18, 209:215))
sbh_H_C18 <- sbh_H_C18[order(sbh_H_C18$C18, decreasing = T),] #C18이 많은 순으로 ordering
table(sbh_H_C18$G1) 
  # HL인 사람 26명

# C18 중 연령별 해당 산업 내 지출 항목 소비 빈도 분석 : B74, B75, B76
# a. B74
H_B74 <- sbh_H_C18 %>% group_by(P2) %>% summarise(B74=mean(B74))
ggplot(data = H_B74, aes(x=P2, y=B74))+geom_bar(stat='identity', fill='Turquoise')+labs(x="연령", y="시계점")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 시계점 금액 비교")
  # 꺾은 선 그래프
ggplot(data = H_B74, aes(x=P2, y=B74, group=1))+geom_line(size=2, color='Turquoise')+geom_point(size=4)+labs(x="연령", y="시계점")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 시계점 금액 비교")

# b. B75
H_B75 <- sbh_H_C18 %>% group_by(P2) %>% summarise(B75=mean(B75))
ggplot(data = H_B75, aes(x=P2, y=B75))+geom_bar(stat='identity', fill='Turquoise')+labs(x="연령", y="귀금속")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 귀금속 금액 비교")
  # 꺾은 선 그래프
ggplot(data = H_B75, aes(x=P2, y=B75, group=1))+geom_line(size=2, color='Turquoise')+geom_point(size=4)+labs(x="연령", y="귀금속")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 귀금속 금액 비교")

# c. B76
H_B76 <- sbh_H_C18 %>% group_by(P2) %>% summarise(B76=mean(B76))
ggplot(data = H_B76, aes(x=P2, y=B76))+geom_bar(stat='identity', fill='Turquoise')+labs(x="연령", y="악세사리")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 악세사리 금액 비교")
  # 꺾은 선 그래프
ggplot(data = H_B76, aes(x=P2, y=B76, group=1))+geom_line(size=2, color='Turquoise')+geom_point(size=4)+labs(x="연령", y="악세사리")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 악세사리 금액 비교")



# 2.4.1. (3) C27

quantile(sbh_H$C27, probs=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) #95%, 429000
length(which(sbh_H$C27 >= 429000)) #상위 5% 4453명
#상위 5% 추출
sbh_H_C27 <- sbh_H %>% filter(C27 >= 429000) %>% select(c(1:8, B161, B162, B163, B164, C1, C27, 209:215))
sbh_H_C27 <- sbh_H_C27[order(sbh_H_C27$C27, decreasing = T),] #C18이 많은 순으로 ordering
table(sbh_H_C27$G1) #HL인 사람 39명

# C27 중 연령별 해당 산업 내 지출 항목 소비 빈도 분석 : B161, B162, B163, B164
# a. B161
H_B161 <- sbh_H_C27 %>% group_by(P2) %>% summarise(B161=mean(B161))
  # 꺾은 선 그래프
ggplot(data = H_B161, aes(x=P2, y=B161, group=1))+geom_line(size=2, color='LightSlateBlue')+geom_point(size=4)+labs(x="연령", y="주유소")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 주유소 금액 비교")

# b. B162
H_B162 <- sbh_H_C27 %>% group_by(P2) %>% summarise(B162=mean(B162))
  # 꺾은 선 그래프
ggplot(data = H_B162, aes(x=P2, y=B162, group=1))+geom_line(size=2, color='LightSlateBlue')+geom_point(size=4)+labs(x="연령", y="LPG가스")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 LPG가스 금액 비교")

# c. B163
H_B163 <- sbh_H_C27 %>% group_by(P2) %>% summarise(B163=mean(B163))
  # 꺾은 선 그래프
ggplot(data = H_B163, aes(x=P2, y=B163, group=1))+geom_line(size=2, color='LightSlateBlue')+geom_point(size=4)+labs(x="연령", y="유류도매")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 유류도매 금액 비교")

# d. B164
H_B164 <- sbh_H_C27 %>% group_by(P2) %>% summarise(B164=mean(B164))
  # 꺾은 선 그래프
ggplot(data = H_B164, aes(x=P2, y=B164, group=1))+geom_line(size=2, color='LightSlateBlue')+geom_point(size=4)+labs(x="연령", y="가정용연료")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 가정용연료 금액 비교")


# 2.4.2. sbh_HL_L에서 양의 상관관계 보인 산업 (C4, C16) ####

# 2.4.2. (1) C4

quantile(sbh_HL_L$C4, probs=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) #95%, 513700
length(which(sbh_HL_L$C4 >= 513700)) #상위 5% 337명
  # 상위 5% 추출
sbh_HLL_C4 <- sbh_HL_L %>% filter(C4 >= 513700) %>% select(c(1:8,	
                                                             B11, B118, B119, B133, B135, B157, B158, B159, B160, B165, B166, B12, B106, B107 , C1, C4, 209:215))
sbh_HLL_C4 <- sbh_HLL_C4[order(sbh_HLL_C4$C4, decreasing = T),] 
  # C4가 많은 순으로 ordering
table(sbh_HLL_C4$G1) 
  # HL인 사람 11명, L인 사람 326명

# a. B11
H_B11 <- sbh_HLL_C4 %>% group_by(P2) %>% summarise(B11=mean(B11))
  # 꺾은 선 그래프
ggplot(data = H_B11, aes(x=P2, y=B11, group=1))+geom_line(size=2, color='OliveDrab')+geom_point(size=4)+labs(x="연령", y="택시")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 택시 금액 비교")

# b. B119
H_B119 <- sbh_HLL_C4 %>% group_by(P2) %>% summarise(B119=mean(B119))
  # 꺾은 선 그래프
ggplot(data = H_B119, aes(x=P2, y=B119, group=1))+geom_line(size=2, color='OliveDrab')+geom_point(size=4)+labs(x="연령", y="택시")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 택시 금액 비교")

# c. B135 주차장
H_B135 <- sbh_HLL_C4 %>% group_by(P2) %>% summarise(B135=mean(B135))
  # 꺾은 선 그래프
ggplot(data = H_B135, aes(x=P2, y=B135, group=1))+geom_line(size=2, color='OliveDrab')+geom_point(size=4)+labs(x="연령", y="주차장")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 주차장 금액 비교")

#c. B159
H_B159 <- sbh_HLL_C4 %>% group_by(P2) %>% summarise(B159=mean(B159))
  # 꺾은 선 그래프
ggplot(data = H_B159, aes(x=P2, y=B159, group=1))+geom_line(size=2, color='OliveDrab')+geom_point(size=4)+labs(x="연령", y="차량용품")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 차량용품 금액 비교")

# d. B165
H_B165 <- sbh_HLL_C4 %>% group_by(P2) %>% summarise(B165=mean(B165))
  # 꺾은 선 그래프
ggplot(data = H_B165, aes(x=P2, y=B165, group=1))+geom_line(size=2, color='OliveDrab')+geom_point(size=4)+labs(x="연령", y="오토바이")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 오토바이 금액 비교")

# e. B166
H_B166 <- sbh_HLL_C4 %>% group_by(P2) %>% summarise(B166=mean(B166))
  # 꺾은 선 그래프
ggplot(data = H_B166, aes(x=P2, y=B166, group=1))+geom_line(size=2, color='OliveDrab')+geom_point(size=4)+labs(x="연령", y="자전거")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 자전거 금액 비교")



#(2)C16
quantile(sbh_HL_L$C16, probs=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) #95%, 239350 
length(which(sbh_HL_L$C16 >= 239350)) #상위 5% 337명
#상위 5% 추출
sbh_HLL_C16 <- sbh_HL_L %>% filter(C4 >= 239350) %>% select(c(1:8,B61, B62, B134, C1, C16, 209:215))
sbh_HLL_C16 <- sbh_HLL_C16[order(sbh_HLL_C16$C16, decreasing = T),] #C4가 많은 순으로 ordering
table(sbh_HLL_C16$G1) #HL인 사람 25명, L인 사람 639명

#분석할 것 B61, B62, B134
#a. B61
H_B61 <- sbh_HLL_C16 %>% group_by(P2) %>% summarise(B61=mean(B61))
#꺾은 선 그래프
ggplot(data = H_B61, aes(x=P2, y=B61, group=1))+geom_line(size=2, color='NavyBlue')+geom_point(size=4)+labs(x="연령", y="통신요금_이동시내전화")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 통신요금_이동시내전화 금액 비교")

#b. B62
H_B62 <- sbh_HLL_C16 %>% group_by(P2) %>% summarise(B62=mean(B62))
#꺾은 선 그래프
ggplot(data = H_B62, aes(x=P2, y=B62, group=1))+geom_line(size=2, color='NavyBlue')+geom_point(size=4)+labs(x="연령", y="통신요금_PC통신_무선호출")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 통신요금_PC통신_무선호출 금액 비교")

#c. B134
H_B134 <- sbh_HLL_C16 %>% group_by(P2) %>% summarise(B134=mean(B134))
#꺾은 선 그래프
ggplot(data = H_B134, aes(x=P2, y=B134, group=1))+geom_line(size=2, color='NavyBlue')+geom_point(size=4)+labs(x="연령", y="유선TV출")+theme(axis.text.x=element_text(angle=90, hjust=1))+ggtitle("연령별 유선TV 금액 비교")


# 2.5. 고객 리볼빙 연체대금 상환 여부 예측모델 ####

library(rpart)
# CART : Classification and Regression Tree, method = "class"
library(rpart.plot)

sbh_HH <- sbh_H %>% filter(G1 == "HH")
summary(sbh_HH$nE6)
summary(sbh_HH$nE5)
ggplot(data = sbh_HH, aes(x = 1, y = nE6)) + geom_boxplot(width = 0.8, outlier.size = 3, outlier.shpe = 16, outlier.color = "red") + geom_violin()
# nE6에 대한 범주형 파생변수 G6 생성 기준

sbh_HH <- sbh_H %>% mutate(G6 = case_when(nE6 <= 5 ~ "l", nE6 <= 12 ~ "h"))
sbh_HH$G6 <- as.factor(sbh_HH$G6)

# 2.5.1. 상환 리스크 있는 리볼빙 위험등급 고객 데이터 프레임 작성_H version and L version ####

# 설명변수(IV)를 기반으로 목표변수(DV) : 상환 Yes / No
# 목표변수와 연관된 종속변수(DV) : nE6 리볼빙 신판 월 100만 원 이용고객의 l(low) & h(high) 등급 정의(G6)

sbh_HH <- sbh_H %>% filter(G1 == "HH")
summary(sbh_HH$nE6)
summary(sbh_HH$nE5)
ggplot(data = sbh_HH, aes(x = 1, y = nE6)) + geom_boxplot(width = 0.8, outlier.size = 3, outlier.shpe = 16, outlier.color = "red") + geom_violin()
# nE6에 대한 범주형 파생변수 G6 생성 기준

sbh_HH <- sbh_H %>% mutate(G6 = case_when(nE6 <= 5 ~ "l", nE6 <= 12 ~ "h"))
sbh_HH$G6 <- as.factor(sbh_HH$G6)
sbh_HH_ID <- sbh_HH %>% select(ID, G6, P2, P4, P5, B167, C1, B12, B106, B107, B74, B75, B76, B161, B162, B163, B164)
sbh_HH <- sbh_HH %>% select(G6, P2, P4, P5, B167, C1, B12, B106, B107, B74, B75, B76, B161, B162, B163, B164)

# 의사결정 분류나무 모델링에 활용할 데이터프레임

# 2.5.2. 훈련용, 검증용 데이터 분리 : partition 7:3 ####
  set.seed(1)
  train_sbh_HH <- sample(c(1:dim(sbh_HH)[1]),
                         dim(sbh_HH)[1] * 0.7)
  train.sbh.HH <- sbh_HH[train_sbh_HH, ]
  valid.sbh.HH <- sbh_HH[-train_sbh_HH, ]
  
  # 분류나무 그래프 
  # 리볼빙 신판 100만 원 이용등급 | 연령 | 카드vip | 금투 | 취급금액 | 이용건수
  
  temp <- table(sbh_HH$G6)
  prop.table(temp)
  # high 0.489618 : low 0.510382 
  
  default.H <- rpart(G6 ~ P2 + P4 + P5 + B167 + C1 + B12 + B106 + B107 
                     + B74 + B75 + B76
                     + B161 + B162 + B163 + B164,
                     data = train.sbh.HH,
                     method = "class")
  # classification tree
  
  summary(default.H)
  library(rpart.plot)
  rpart.plot(default.H)
  library(rattle)
  fancyRpartPlot(default.H)
  # 분류나무 시각화
  
  predict.H <- predict(default.H, valid.sbh.HH, type = "class")
  library(caret)
  confusionMatrix(predict.H, valid.sbh.HH$G6)
  # 혼동행렬/정오표 : 트리모델 학습 성능평가행렬
  
  # 정확도(Accuracy) : 95% 신뢰구간 내 77.98% ~ 78.97%
  # 민감도(Sensitivity) : 77.41% Y 옳게 예측
  # 특이도(Specificity) : 79.5% N 옳게 예측
  
  # 신용카드 리볼빙 대금 상환 불가 고객 분석
  default1 <- sbh_HH_ID %>% filter(B167 >= 1015000 & P4 >= 0.5 & C1 >= 45 & B161 >= 83500 & B107 >=9500)
  
  default2 <- default1 %>% filter(P4 >= 0.5 & B167 >= 1605000 & C1 >= 45 & B161 >= 177500)
  
  sbh_Default <- inner_join(default2, sbh, by = "ID")
  
  # Receiver Operator Characteristic Curve
  '''
  printcp(default.H)
  pruned.H <- prune(default.H, cp = 0.1)
  prunedPred.H <- predict(pruned.H, valid.sbh.HH, type = "class")
  rpart.plot(pruned.H)
  confusionMatrix(prunedPred.H, valid.sbh.HH$G6)
  '''
  # 가지치기(Prunning) : 오히려 정확도 하락, 특이도 하락, 민감도만 상승한 결과
  
  # 랜덤 포레스트 #
  '''
  library(randomForest)
  rf.H <- randomForest(G6 ~ P2 + P4 + P5 + B167 + C1, data = train.sbh.HH)
  rf.H
  pred.rf.H <- predict(rf.H, valid.sbh.HH)
  confusionMatrix(pred.rf.H, valid.sbh.HH$G6)
  
  # ROC 커브 : 의사결정나무, 랜덤포레스트 성능 비교
  library(EPI)
  tree_ROC <- ROC(form=G6~predict.H, data=valid.sbh.HH, plot="ROC") # 의사결정나무
  rf_ROC <- ROC(form=G6~pred.rf.H, data=valid.sbh.HH, plot="ROC") # 랜덤포레스트

