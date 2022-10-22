### *** 아이디어 트랙 2 *** ###

# 3. 고객 유치 아이디어 ####
# 3.1. vip 고객 대상 서비스 - k-means 이용 ####

# 3.1.1 고객 성향으로 군집 분석 - 이용건수, 취급금액으로 군집 설정 ####
# sampling X, 전체 vip로, 267378명

sbh_vip <- sbh %>% filter(P4 == "1") %>% filter(G1 != "HH" | G1 != "HL") %>% select(ID, C1, B167)

# 표준화 - 이용건수와 취급금액의 단위가 달라서
vip_scaled <- as.data.frame(scale(sbh_vip[,2:3], center=TRUE, scale = TRUE))
vip_scaled$ID <- sbh_vip$ID
head(vip_scaled)

# 최적 K 찾기 - Elbow Method

install.packages("factoextra") 
library(factoextra)

vip_n <- sbh_vip[sample(nrow(sbh_vip),10000),]
vip_train <- sample_frac(vip_n, size=0.5)
vip_test <- sample_frac(vip_n, size=0.5)

fviz_nbclust(vip_train, kmeans, method = "wss", k.max = 15) + 
  theme_minimal() + 
  ggtitle("Elbow Method")
 
# k-means 군집분석 진행
set.seed(100000)
k_vip<-kmeans(vip_scaled[,1:2],4) #k=4
k_vip

# ggplot2
library(ggplot2)
ggplot(data=vip_scaled, aes(x=C1, y=B167, colour=k_vip$cluster))+geom_point(shape=19, size=3)


# 군집별 분류
sbh_vip$cluster <- k_vip$cluster

# (1) vip 1
sbh_vip_1 <- sbh_vip %>% filter(cluster == "1")
sbh_vip_1 <- inner_join(sbh, sbh_vip_1, by="ID")
#sbh_vip_1[, 9:174] <- ifelse(sbh_vip_1[, 9:174] != 0, 'yes', 'no')
sbh_vip_1[, 9:174] <- ifelse(sbh_vip_1[, 9:174] != 0, '1', '0') #산업으로 할 때
sbh_vip_1$B34 <- NULL
sbh_vip_1$B167.x <- NULL
sbh_vip_1$B167.y <- NULL
sbh_vip_1$C1.x <- NULL
sbh_vip_1$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_vip_1[,i] <- as.numeric(sbh_vip_1[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_vip_1 <- sbh_vip_1 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164
)

names(sbh_vip_1)
sbh_vip_1[, 180:205] <- ifelse(sbh_vip_1[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_vip_1[,i] <- as.factor(sbh_vip_1[,i])
}


# (2) vip2
sbh_vip_2 <- sbh_vip %>% filter(cluster == "2")
sbh_vip_2 <- inner_join(sbh, sbh_vip_2, by="ID")
sbh_vip_2[, 9:174] <- ifelse(sbh_vip_2[, 9:174] != 0, 1, 0)
sbh_vip_2$B34 <- NULL
sbh_vip_2$B167.x <- NULL
sbh_vip_2$B167.y <- NULL
sbh_vip_2$C1.x <- NULL
sbh_vip_2$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_vip_2[,i] <- as.numeric(sbh_vip_2[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_vip_2 <- sbh_vip_2 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_vip_2[, 180:205] <- ifelse(sbh_vip_2[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_vip_2[,i] <- as.factor(sbh_vip_2[,i])
}


# (3) vip3
sbh_vip_3 <- sbh_vip %>% filter(cluster == "3")
sbh_vip_3 <- inner_join(sbh, sbh_vip_3, by="ID")
sbh_vip_3[, 9:174] <- ifelse(sbh_vip_3[, 9:174] != 0, 1, 0)
sbh_vip_3$B34 <- NULL
sbh_vip_3$B167.x <- NULL
sbh_vip_3$B167.y <- NULL
sbh_vip_3$C1.x <- NULL
sbh_vip_3$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_vip_3[,i] <- as.numeric(sbh_vip_3[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_vip_3 <- sbh_vip_3 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_vip_3[, 180:205] <- ifelse(sbh_vip_3[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_vip_3[,i] <- as.factor(sbh_vip_3[,i])
}


# (4) vip4 
sbh_vip_4 <- sbh_vip %>% filter(cluster == "4")
sbh_vip_4 <- inner_join(sbh, sbh_vip_4, by="ID")
sbh_vip_4[, 9:174] <- ifelse(sbh_vip_4[, 9:174] != 0, 1, 0)
sbh_vip_4$B34 <- NULL
sbh_vip_4$B167.x <- NULL
sbh_vip_4$B167.y <- NULL
sbh_vip_4$C1.x <- NULL
sbh_vip_4$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_vip_4[,i] <- as.numeric(sbh_vip_4[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_vip_4 <- sbh_vip_4 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_vip_4[, 180:205] <- ifelse(sbh_vip_4[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_vip_4[,i] <- as.factor(sbh_vip_4[,i])
}


# 3.1.2.연관 규칙 마이닝 ####

library(arules)

# (1) vip1 연관규칙
sbh_vip_1_s <- sbh_vip_1[sample(nrow(sbh_vip_1),100),] #sampling
sbh_vip1s_c <- subset(sbh_vip_1_s, select = c(180:205))
#str(sbh_vip1s_c)
#head(sbh_vip1s_c)

trans1 <-as(sbh_vip1s_c, "transactions")

# 연관규칙 설정
rule_1 <- apriori(data = trans1, parameter = list(support = 0.3, confidence = 0.75))
# summary(rule_1)
# 연관 규칙 확인, 분석
inspect_1 <- inspect(head(sort(rule_1, by = "confidence"), n = 100))

inspect_1$lhs_tf <- str_detect(inspect_1$lhs, "yes")
inspect_1$rhs_tf <- str_detect(inspect_1$rhs, "yes")

inspect_1$lhs_tf <- ifelse(inspect_1$lhs_tf == 'TRUE' , 'yes', 'no')
inspect_1$rhs_tf <- ifelse(inspect_1$rhs_tf == 'TRUE' , 'yes', 'no')

#inspect_1$lhs_tf <- as.factor(inspect_1$lhs_tf)
#inspect_1$rhs_tf <- as.factor(inspect_1$rhs_tf)
#inspect_1 %>% filter(lhs_tf == "yes")

which(inspect_1$lhs_tf == "yes" & inspect_1$rhs_tf == "yes") #[1]  1 17 20 32 37 45 47
inspect_1_f <- inspect_1[c(1,17,20,32,37,45,47),]


# (2) vip2  연관규칙
sbh_vip_2_s <- sbh_vip_2[sample(nrow(sbh_vip_2),100),] #sampling
sbh_vip2s_c<- subset(sbh_vip_2_s, select = c(180:205))
#str(sbh_vip1s_c)
#head(sbh_vip1s_c)

trans2<-as(sbh_vip2s_c, "transactions")

# 연관규칙 설정
rule_2 <- apriori(data = trans2, parameter = list(support = 0.3, confidence = 0.75))
#summary(rule_2)
# 연관 규칙 확인, 분석
inspect_2 <- inspect(head(sort(rule_2, by = "confidence"), n = 100))

inspect_2$lhs_tf <- str_detect(inspect_2$lhs, "yes")
inspect_2$rhs_tf <- str_detect(inspect_2$rhs, "yes")

inspect_2$lhs_tf <- ifelse(inspect_2$lhs_tf == 'TRUE' , 'yes', 'no')
inspect_2$rhs_tf <- ifelse(inspect_2$rhs_tf == 'TRUE' , 'yes', 'no')

which(inspect_2$lhs_tf == "yes" & inspect_2$rhs_tf == "yes") #  [1]   2   6  50  51  52  54  55  57  59  78  81  85  88  91  94  97 100
inspect_2_f <- inspect_2[c( 2,  6, 50, 51,52, 54, 55, 57, 59, 78,  81,  85,  88,  91,  94,  97, 100),]


# (3) vip3 연관규칙
sbh_vip_3_s <- sbh_vip_3[sample(nrow(sbh_vip_3),100),] #sampling
sbh_vip3s_c<- subset(sbh_vip_3_s, select = c(180:205))
#str(sbh_vip1s_c)
#head(sbh_vip1s_c)

trans3<-as(sbh_vip3s_c, "transactions")

# 연관규칙 설정
rule_3 <- apriori(data = trans3, parameter = list(support = 0.3, confidence = 0.75))
#summary(rule_2)
# 연관 규칙 확인, 분석
inspect_3 <- inspect(head(sort(rule_3, by = "confidence"), n = 100))

inspect_3$lhs_tf <- str_detect(inspect_3$lhs, "yes")
inspect_3$rhs_tf <- str_detect(inspect_3$rhs, "yes")

inspect_3$lhs_tf <- ifelse(inspect_3$lhs_tf == 'TRUE' , 'yes', 'no')
inspect_3$rhs_tf <- ifelse(inspect_3$rhs_tf == 'TRUE' , 'yes', 'no')

which(inspect_3$lhs_tf == "yes" & inspect_3$rhs_tf == "yes") # [1] 13 32
inspect_3_f <- inspect_3[c(13, 32),]


# (4) vip4 연관규칙
sbh_vip_4_s <- sbh_vip_4[sample(nrow(sbh_vip_4),100),] #sampling
sbh_vip4s_c<- subset(sbh_vip_4_s, select = c(180:205))

# str(sbh_vip1s_c)
# head(sbh_vip1s_c)

trans4 <- as(sbh_vip4s_c, "transactions")

# 연관규칙 설정
rule_4 <- apriori(data = trans4, parameter = list(support = 0.3, confidence = 0.75))
#summary(rule_2)
# 연관 규칙 확인, 분석
inspect_4 <- inspect(head(sort(rule_4, by = "confidence"), n = 100))

inspect_4$lhs_tf <- str_detect(inspect_4$lhs, "yes")
inspect_4$rhs_tf <- str_detect(inspect_4$rhs, "yes")

inspect_4$lhs_tf <- ifelse(inspect_4$lhs_tf == 'TRUE' , 'yes', 'no')
inspect_4$rhs_tf <- ifelse(inspect_4$rhs_tf == 'TRUE' , 'yes', 'no')

which(inspect_4$lhs_tf == "yes" & inspect_4$rhs_tf == "yes") #[1]  2  3  4  5  6  7  8  9 10 12 13 14 15 16 19 20 21 22 23 24 25 26 28 29 30 31 32 33 34 35 36 37 39 40 42 43 44 45 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 67 68 69 70 72 73 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 93 94 95 97 98 99
inspect_4_f <- inspect_4[c(2,  3,  4,  5,  6, 7,  8,  9 ,10, 12, 13, 14, 15, 16, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 39, 40, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 67, 68, 69, 70, 72, 73, 75, 76, 77, 78,
                           79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 93, 94, 95, 97, 98, 99),]




# 3.2 vip 제외 전체 고객 대상 서비스 - k-means 이용 ####
sbh_n_vip <- sbh %>% filter(P4 == "0") %>% filter(G1 != "HH" | G1 != "HL") %>% select(ID, C1, B167)

# 표준화 - 이용건수와 취급금액의 단위가 달라서
n_vip_scaled <- as.data.frame(scale(sbh_n_vip[,2:3], center=T, scale = T))
n_vip_scaled$ID <- sbh_n_vip$ID
head(n_vip_scaled)

# k-means 군집분석 진행
set.seed(100000)
k_nvip<-kmeans(n_vip_scaled[,1:2],4) #k=4
k_nvip

plot(n_vip_scaled[,1], n_vip_scaled[,2], xlab="이용건수", ylab="취급금액", pch=21, col=k_nvip$cluster)

# ggplot2
ggplot(data=n_vip_scaled, aes(x=C1, y=B167, colour=k_nvip$cluster))+geom_point(shape=19, size=3)
#h+ scale_fill_brewer(palette="YlOrBr")


# 군집별 분류
sbh_n_vip$cluster <- k_nvip$cluster

# (1) nvip 1 
sbh_nvip_1 <- sbh_n_vip %>% filter(cluster == "1")
sbh_nvip_1 <- inner_join(sbh, sbh_nvip_1, by="ID")

sbh_nvip_1[, 9:174] <- ifelse(sbh_nvip_1[, 9:174] != 0, 1, 0) #B 범주화, 금액이 0이 아니면 1로 

sbh_nvip_1$B34 <- NULL
sbh_nvip_1$B167.x <- NULL
sbh_nvip_1$B167.y <- NULL
sbh_nvip_1$C1.x <- NULL
sbh_nvip_1$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_nvip_1[,i] <- as.numeric(sbh_nvip_1[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_nvip_1 <- sbh_nvip_1 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_nvip_1[, 180:205] <- ifelse(sbh_nvip_1[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_nvip_1[,i] <- as.factor(sbh_nvip_1[,i])
}


# (2) nvip2 
sbh_nvip_2 <- sbh_n_vip %>% filter(cluster == "2")
sbh_nvip_2 <- inner_join(sbh, sbh_nvip_2, by="ID")

sbh_nvip_2[, 9:174] <- ifelse(sbh_nvip_2[, 9:174] != 0, 1, 0) #B 범주화, 금액이 0이 아니면 1로 

sbh_nvip_2$B34 <- NULL
sbh_nvip_2$B167.x <- NULL
sbh_nvip_2$B167.y <- NULL
sbh_nvip_2$C1.x <- NULL
sbh_nvip_2$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_nvip_2[,i] <- as.numeric(sbh_nvip_2[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_nvip_2 <- sbh_nvip_2 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_nvip_2[, 180:205] <- ifelse(sbh_nvip_2[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_nvip_2[,i] <- as.factor(sbh_nvip_2[,i])
}


# (3) nvip3 
sbh_nvip_3 <- sbh_n_vip %>% filter(cluster == "3")
sbh_nvip_3 <- inner_join(sbh, sbh_nvip_3, by="ID")

sbh_nvip_3[, 9:174] <- ifelse(sbh_nvip_3[, 9:174] != 0, 1, 0) #B 범주화, 금액이 0이 아니면 1로 

sbh_nvip_3$B34 <- NULL
sbh_nvip_3$B167.x <- NULL
sbh_nvip_3$B167.y <- NULL
sbh_nvip_3$C1.x <- NULL
sbh_nvip_3$C1.y <- NULL

#numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_nvip_3[,i] <- as.numeric(sbh_nvip_3[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_nvip_3 <- sbh_nvip_3 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_nvip_3[, 180:205] <- ifelse(sbh_nvip_3[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_nvip_3[,i] <- as.factor(sbh_nvip_3[,i])
}


# (4) nvip 4 
sbh_nvip_4 <- sbh_n_vip %>% filter(cluster == "4")
sbh_nvip_4 <- inner_join(sbh, sbh_nvip_4, by="ID")

sbh_nvip_4[, 9:174] <- ifelse(sbh_nvip_4[, 9:174] != 0, 1, 0)

sbh_nvip_4$B34 <- NULL
sbh_nvip_4$B167.x <- NULL
sbh_nvip_4$B167.y <- NULL
sbh_nvip_4$C1.x <- NULL
sbh_nvip_4$C1.y <- NULL

# numeric으로 바꾸기 - 산업으로 할 때
for (i in (9:173)){
  sbh_nvip_4[,i] <- as.numeric(sbh_nvip_4[,i])
}

# 산업으로 분류하기 - 산업으로 마이닝 진행
sbh_nvip_4 <- sbh_nvip_4 %>% mutate(
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
  C26 = B139 + B140 + B141 + B143 +  B144 + B145 + B146 + B147 + B148 + B149,
  C27 = B161 + B162 + B163 + B164, 
)

sbh_nvip_4[, 180:205] <- ifelse(sbh_nvip_4[, 180:205] != 0, 'yes', 'no')

# factor로 바꾸기
for (i in (180:205)){
  sbh_nvip_4[,i] <- as.factor(sbh_nvip_4[,i])
}


# 3.2.2 연관 규칙 마이닝 ####

# (1) nvip 연관규칙 
library(arules)
sbh_nvip_1_s <- sbh_nvip_1[sample(nrow(sbh_nvip_1),100),] #sampling
sbh_nvip1s_c<- subset(sbh_vip_1_s, select = c(180:205))
#str(sbh_nvip1s_c)
#head(sbh_nvip1s_c)

ntrans1<-as(sbh_nvip1s_c, "transactions")

# 연관규칙 설정
nrule_1 <- apriori(data = ntrans1, parameter = list(support = 0.3, confidence = 0.75))
#summary(rule_1)
# 연관 규칙 확인, 분석
ninspect_1 <- inspect(head(sort(nrule_1, by = "confidence"), n = 100))

ninspect_1$lhs_tf <- str_detect(ninspect_1$lhs, "yes")
ninspect_1$rhs_tf <- str_detect(ninspect_1$rhs, "yes")

ninspect_1$lhs_tf <- ifelse(ninspect_1$lhs_tf == 'TRUE' , 'yes', 'no')
ninspect_1$rhs_tf <- ifelse(ninspect_1$rhs_tf == 'TRUE' , 'yes', 'no')

which(ninspect_1$lhs_tf == "yes" & ninspect_1$rhs_tf == "yes") #[1] 16
ninspect_1_f <- ninspect_1[16,]


# (2) nvip 연관분석

library(arules)
sbh_nvip_2_s <- sbh_nvip_2[sample(nrow(sbh_nvip_2),100),] #sampling
sbh_nvip2s_c<- subset(sbh_vip_2_s, select = c(180:205))
#str(sbh_nvip2s_c)
#head(sbh_nvip2s_c)

ntrans2<-as(sbh_nvip2s_c, "transactions")

# 연관규칙 설정
nrule_2 <- apriori(data = ntrans2, parameter = list(support = 0.3, confidence = 0.75))
#summary(rule_1)
# 연관 규칙 확인, 분석
ninspect_2 <- inspect(head(sort(nrule_2, by = "confidence"), n = 100))

ninspect_2$lhs_tf <- str_detect(ninspect_2$lhs, "yes")
ninspect_2$rhs_tf <- str_detect(ninspect_2$rhs, "yes")

ninspect_2$lhs_tf <- ifelse(ninspect_2$lhs_tf == 'TRUE' , 'yes', 'no')
ninspect_2$rhs_tf <- ifelse(ninspect_2$rhs_tf == 'TRUE' , 'yes', 'no')

which(ninspect_2$lhs_tf == "yes" & ninspect_2$rhs_tf == "yes") # [1]   2   6  50  51  52  54  55  57  59  78  81  85  88  91  94  97 100
ninspect_2_f <- ninspect_2[c(2, 6, 50,  51,  52,  54,  55,  57,  59, 78,  81,  85,  88,  91,  94,  97, 100),]


# (3) nvip3 연관분석
library(arules)
sbh_nvip_3_s <- sbh_nvip_3[sample(nrow(sbh_nvip_3),100),] #sampling
sbh_nvip3s_c<- subset(sbh_vip_3_s, select = c(180:205))
#str(sbh_nvip3s_c)
#head(sbh_nvip3s_c)

ntrans3<-as(sbh_nvip3s_c, "transactions")

# 연관규칙 설정
nrule_3 <- apriori(data = ntrans3, parameter = list(support = 0.3, confidence = 0.75))
# 연관 규칙 확인, 분석
ninspect_3 <- inspect(head(sort(nrule_3, by = "confidence"), n = 100))

ninspect_3$lhs_tf <- str_detect(ninspect_3$lhs, "yes")
ninspect_3$rhs_tf <- str_detect(ninspect_3$rhs, "yes")

ninspect_3$lhs_tf <- ifelse(ninspect_3$lhs_tf == 'TRUE' , 'yes', 'no')
ninspect_3$rhs_tf <- ifelse(ninspect_3$rhs_tf == 'TRUE' , 'yes', 'no')

which(ninspect_3$lhs_tf == "yes" & ninspect_3$rhs_tf == "yes") # [1] 13 32
ninspect_3_f <- ninspect_3[c(13, 32),]


# (2) nvip4 연관분석

library(arules)
sbh_nvip_4_s <- sbh_nvip_4[sample(nrow(sbh_nvip_4),100),] #sampling
sbh_nvip4s_c<- subset(sbh_vip_4_s, select = c(180:205))
#str(sbh_vip1s_c)
#head(sbh_vip1s_c)

ntrans4<-as(sbh_nvip4s_c, "transactions")

# 연관규칙 설정
nrule_4 <- apriori(data = ntrans4, parameter = list(support = 0.3, confidence = 0.75))
#summary(rule_1)
# 연관 규칙 확인, 분석
ninspect_4 <- inspect(head(sort(nrule_4, by = "confidence"), n = 100))

ninspect_4$lhs_tf <- str_detect(ninspect_4$lhs, "yes")
ninspect_4$rhs_tf <- str_detect(ninspect_4$rhs, "yes")

ninspect_4$lhs_tf <- ifelse(ninspect_4$lhs_tf == 'TRUE' , 'yes', 'no')
ninspect_4$rhs_tf <- ifelse(ninspect_4$rhs_tf == 'TRUE' , 'yes', 'no')

which(ninspect_4$lhs_tf == "yes" & ninspect_4$rhs_tf == "yes") 
#[1]  2  3  4  5  6  7  8  9 10 12 13 14 15 16 19 20 21 22 23 24 25 26 28 29 30 31 32 33 34 35 36 37 39 40 42 43 44 45
#[39] 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 67 68 69 70 72 73 75 76 77 78 79 80 81 82 83 84 85 86 87
#[77] 88 89 90 91 93 94 95 97 98 99
ninspect_4_f <- ninspect_4[c(2,  3,  4,  5,  6,  7,  8,  9, 10, 12, 13, 14, 15, 16, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 39, 40, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 67, 68, 69, 70, 72, 73, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 93, 94, 95, 97, 98, 99),]


# 3.3 nvip 중 신한 아니고 B은행(기타)인 사람 filtering - push 보낼 사람 ####
sbh_n_vip$P7
sbh_push <- sbn_n_vip %>% filter(P7=="B")

