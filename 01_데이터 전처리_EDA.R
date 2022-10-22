### 신한금융그룹 빅데이터 해커톤 ###
### 숙데콘(숙명데이터유니콘)의 신한카드 데이터 분석 프로젝트 ###

## 1. ** 데이터 전처리 ~ 탐색적 데이터 분석 ** ####


install.packages("readr")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("caret")
install.packages("randomForest")

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(rpart)
library(caret)


# 1.1. 데이터 불러오기

library(readr)
sbh <- read_csv("data_007.csv", col_names = TRUE, na = "NA", locale = locale("ko", encoding = "euc-kr"))
str(sbh)
sbh <- as.data.frame(sbh)


# 1.2. 거래패턴 코드 E1 ~ E6 데이터 길이 12자리 여부 확인 : 이상 없음


# 1.3. 고객 맞춤형 리스크 지표 #

# 1.3.1. 자료형 변환(char ~ fac) : P1(성별), P2(나이), P7(거래금융기관)

for (i in 1:(dim(sbh))[2]) {
  if(class(sbh[,i]) == "character") {
    sbh[,i] <- as.factor(sbh[,i])
  }
}
str(sbh$P1)
str(sbh$P2)
str(sbh$P7)

# 1.3.2. 결제정보 168개 열의 이상치 시각화

library(ggplot2)
boxplot(sbh$B20)$stat
mean(sbh$B20) + 2.58*sd(sbh$B20)
table(sbh$B20 > mean(sbh$B20) + 2.58*sd(sbh$B20)) 
  # 백화점 열 내 이상치 수
table(sbh$B42 > mean(sbh$B42) + 3*sd(sbh$B42)) 
  # 한식 열 내 이상치

norm(sbh$B42)
  # Error : class의 문제
shapiro.test(sample(sbh$B42, size = 5000, replace = FALSE))
  # 한식 열 내 데이터 정규성 조건 검토 : 만족하지 않음
x <- sample(sbh$B42, size = 5000, replace = FALSE)
plot(dnorm(x, mean = 0, sd = 1))
  # 한식 열 데이터의 표준정규분포도

sbh$ID <- c(1:473225)
sbh <- sbh %>% relocate(ID, .before = P1) 
  # 고객 번호 변수 생성

ggplot(data = sbh, aes(x = ID, y = B20)) + geom_point()
  # 고객 번호 열과 백화점 열 데이터의 산점도
  # 데이터 간 관계 시각화
table(sbh$B20 > 3e+6)
  # 3e+6 넘는 이상치 수 : 63
sbh[which(sbh$B20 > 3e+6)]
  # 이상치 값
which(sbh$B20 > 3e+6)
  # 이상치의 행렬 내 위치

sbh_B20_out <- sbh[which(sbh$B20 > 3e+6), c(183:188)]
  # 백화점 열(B20)의 이상치 행과 패턴코드 월별 빈도 열(nE1 ~ nE6)에 대한 하위 데이터 프레임 작성
cor.test(sbh_B20_out$nE1, sbh_B20_out$nE6, method = "pearson")
  # 상관분석 : 9.5%(14%에 못 미침)
  # 결론 : 각 패턴코드 6열의 범주형 파생변수는 개별적 기준으로 생성해야 함

# C2 : 숙박(Hospitality)
line_C2 <- ggplot(data = sbh, aes(x = G1, y = C2))
line_C2 + stat_summary(fun.y = mean, geom = "line", aes(group = 1), color = "Blue") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
  # stat_summary()로 그릴 통계량을 평균으로 설정
  # fun.data parameter는 부트스트랩에 기초한(정규분포 가장 하지 않은) 95% 신뢰구간과 geom = "errorbar"라는 기하 객체

# C6 : 유통(Retail and Wholesale)
line_C6 <- ggplot(data = sbh, aes(x = G1, y = C6))
line_C6 + stat_summary(fun.y = mean, geom = "line", aes(group = 1), color = "Blue") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)

# C10 : 외식(Eating out)
line_C10 <- ggplot(data = sbh, aes(x = G1, y = C10))
line_C10 + stat_summary(fun.y = median, geom = "line", aes(group = 1), color = "Blue") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)


# 1.3.3. 패턴코드의 12개월별 거래빈도 파생변수 생성

library(dplyr)
library(stringr)
sbh <- sbh %>% mutate(nE1 = str_count(E1, "1"), nE2 = str_count(E2, "1"), nE3 = str_count(E3, "1"), nE4 = str_count(E4, "1"), nE5 = str_count(E5, "1"), nE6 = str_count(E6, "1"))

sbh$ID <- c(1:473225)
sbh <- sbh %>% relocate(ID, .before = P1)
  # 고객번호 파생변수 생성

# 1.3.4. 결제정보(B열) 산업 분류

  B_Codebook <- data.frame(
    
    Industry_code = c("C2","C3","C4","C5", "C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25", "C26", "C27", "NotC"),
    
    B_code = c("B1, B2, B3, B4, B5", "B6, B7, B8, B9, B10", "B11, B118, B119, B133, B135, B157, B158, B159, B160, B165, B166", "B12, B106, B107", "B13, B14, B15, B16, B18, B21, B32, B33", "B17, B19, B20, B92, B125, B126, B128, B136, B154", "B22, B23, B24, B25, B26, B27, B28", "B29, B30, B31, B151", "B35, B36, B37, B38, B39, B40, B41, B42", "B43, B44, B45, B46, B47, B52, B53", "B48, B49, B50, B51, B56", "B58", "B55 B57, B59, B60, B124, B150", "B54", "B61, B62, B134", "B63, B64, B65, B66, B67, B68, B69, B70, B71, B72, B73", "B74, B75, B76", "B77, B78, B79, B80, B81, B82, B83, B86, B87, B91, B93, B103, B104, B113, B114, B121, B137, B138", "B84, B85, B142", "B88, B94, B109, B110, B111, B112, B115, B116, B117", "B89, B90, B97, B98, B99, B100, B101, B102, B105, B120, B123", "B95, B96, B152, B153", "B108, B122, B155, B156", "B129, B130, B131, B132", "B139, B140, B141, B143, B144, B145, B146, B147, B148, B149", "B161, B162, B163, B164", "B34, B167, C1"), 
    
    Contents = c("숙박(Hospitality)", "여행(Travel)", "이동(Mobility)", "고정지출(Fixed cost)", "유통(Retail and Wholesale)", "생활 서비스(Life service)", "농산품(Agrofood)", "식료품(F&B)", "외식(Eating out)", "인테리어(Interior)", "전자제품(Electronics)", "소프트웨어(Software)", "기자재(Equipment)", "중고(Second-hand)", "전자통신(Electronic communication)", "섬유(Textile)", "고가품(Valuable Goods)", "문화예술(Culture & Art", "반려동물(Companion animal)", "가구/세대(Household)", "레저(Leisure)", "외모관리(Appearance management)", "교육콘텐츠(Educational contents)", "전문 서비스(Specialized service)", "의료(medical treatment)", "가스(gas)", "금액_결제대행_PG, 취급금액, 이용건수"))
  
  sbh <- sbh %>% mutate(
    C2 = B1 + B2 + B3 + B4 + B5,
    C3 = B6 + B7 + B8 + B9 + B10,
    C4 = B11 + B118 + B119 + B133 + B135 + B157 + B158 +  B159 + B160 + B165 + B166 + B12 + B106 + B107,
    C5 = B12 + B106 + B107,
    C6 = B13 + B14 + B15 + B16 + B18 + B21 + B32 + B33, 
    C7 = B17 + B19 + B20 + B92 + B125 + B126 + B128 + B136 + B154,
    C8 = B22 + B23 + B24 + B25 + B26 + B27 + B28,
    C9 = B29 + B30 + B31 +B151,
    C10 = B35 + B36 + B37 + B38 + B39 + B40 + B41 + B42,
    C11 = B43 + B44 + B45 + B46 + B47 + B52 + B53,
    C12 = B48 + B49 + B50 + B51 + B56,
    C13 = B58,
    C14 = B55 + B57 + B59 + B60 + B124 + B150,
    C15 = B54, 
    C16 = B61 + B62 + B134,
    C17 = B63 + B64 + B65 + B66 + B67 + B68 + B69 + B70 +  B71 + B72 + B73,
    C18 = B74 + B75 + B76,
    C19 = B77 + B78 + B79 + B80 + B81 + B82 + B83 + B86 + B87 + B91 + B93 + B103 + B104 + B113 + B114 + B121 + B137 + B138,
    C20 = B84 + B85 + B142,
    C21 = B88 + B94 + B109 + B110 + B111 + B112 + B115 + B116 + B117,
    C22 = B89 + B90 + B97 + B98 + B99 + B100 + B101 + B102 + B105 + B120 + B123,
    C23 = B95 + B96 + B152 + B153,
    C24 = B108 + B122 + B155 + B156,
    C25 = B129 + B130 + B131 + B132,
    C26 = B139 + B140 + B141 + B143 + B144 + B145 + B146 + B147 + B148 + B149,
    C27 = B161 + B162 + B163 + B164, 
  )

sbh <- sbh %>% relocate(c(nE1, nE2, nE3, nE4, nE5, nE6), .after = C27)


# 1.4. 패턴코드 E1에 대한 4가지 범주형 파생변수 생성(N, L, LH, HH) 작성 : G1(E1의 Grade col) 

sbh <- sbh %>% mutate(sE1 = grepl("[1]{3,}", sbh$E1))
sbh$sE1 <- as.factor(sbh$sE1)
length(which(sbh$sE1 == TRUE))
length(which(sbh$sE1 == FALSE))

sbh <- sbh %>% mutate(G1 = case_when(nE1 == 0 ~ "N", nE1 <= 2 ~ "L", nE1 <= 12 & sE1 == FALSE ~ "HL", sE1 == TRUE ~ "HH"))
table(sbh$G1)
sbh$sE1 <- NULL
#sbh$G1 <- as.factor(sbh$G1)

sbh_H <- sbh %>% filter(G1 == "HH" | G1 == "HL")
sbh_H$G1 <- as.factor(sbh_H$G1)
sbh_H$P2 <- as.factor(sbh_H$P2)
str(sbh_H$G1)
str(sbh_H$P2)

sbh %>% group_by(G1) %>% summarise(count = n())
  # 88,419행
  # 1년에 최소 3번 연속 3번 이상 리볼빙한 고객 수로 E1 패턴코드 범주 중 최고위험도 집단으로 해석 
  # 리볼빙(연체예정 카드대금 이월, 수수료 20% 이상)


# 1.5. 모든 산업의 리볼빙 위험 등급별 합계 및 평균

sbh_sub <- sbh %>% select(c(1:8), B34, B167, c(176:214))
  # subset(하위데이터프레임) 작성
  
for (i in (18:43)) {
  print(sum(sbh_sub[, i]) / nrow(sbh_sub))
}
  # for문의 조건문 반복 수행하여 출력값 저장
sum(sbh_sub[, 43]) / nrow(sbh_sub)
sum(sbh_sub$C27) / nrow(sbh_sub)
  # 조건문 수행 오류 확인 : 이상 없음

sbh_C_mean <- data.frame(C = c(2:27), mean = c(9531.889, 14139.02, 253985.4, 218609.6, 308964.3, 68014.2, 34655.95, 11764.84, 200783.7, 10933.54, 27415.42, 11641.79, 2614.359, 153.9944, 58360.24, 15619.71, 1965.49, 13810.65, 5528.275, 8192.251, 36394.58, 18261.34, 36511.76, 3722.379, 181199, 131178.9))

ggplot(data = sbh_C_mean, aes(x = C, y = mean)) + geom_line(col = "blue") + geom_point(shape = 19, alpha = 0.5, col = "blue", size = 3) + scale_y_continuous(labels = scales::comma) + labs(x = "소비데이터 산업분류코드 C2 ~ C27", y = "결제금액 평균값")

line_G1_mean <- ggplot(data = sbh_C_mean, aes(x = C, y = mean)) + geom_line(col = "blue") + geom_point(shape = 19, alpha = 0.5, col = "blue", size = 3) + scale_y_continuous(labels = scales::comma) + labs(x = "소비데이터 산업분류코드 C2 ~ C27", y = "결제금액 평균값")
  # 그래프 저장
