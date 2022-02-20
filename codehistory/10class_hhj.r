#빅데이터A 2차 한호종

rm(list = ls())
.rs.restartR()

#데이터셋 로딩
train <- read.csv("C:/Rwork/airline_dataset/train.csv")
test <- read.csv("C:/Rwork/airline_dataset/test.csv")

dim(train)      #데이터 구조확인
dim(test)
colnames(train) #컬럼 확인
colnames(test)
head(train, 10) #데이터 확인
tail(test, 10)
str(train)      #컴럼별 데이터형 확인
str(test)

#결측치 확인
table(is.na(train)) #결측치 확인
table(is.na(test))

#데이터 전처리1
## id 제거
train_fix <- train[2:24]
test_fix <- test[2:23]

##char형 데이터를 수치형으로 변환
table(train_fix$Gender)
table(test_fix$Gender)
train_fix$Gender <- ifelse(train$Gender == "Male", 1, 2)
test_fix$Gender <- ifelse(test$Gender == "Male", 1, 2)

table(train_fix$Customer.Type)
table(test_fix$Customer.Type)
train_fix$Customer.Type <- ifelse(train$Customer.Type == "disloyal Customer", 1, 2)
test_fix$Customer.Type <- ifelse(test$Customer.Type == "disloyal Customer", 1, 2)

table(train_fix$Type.of.Travel)
table(test_fix$Type.of.Travel)
train_fix$Type.of.Travel <- ifelse(train$Type.of.Travel == "Business travel", 1, 2)
test_fix$Type.of.Travel <- ifelse(test$Type.of.Travel == "Business travel", 1, 2)

table(train_fix$Class)
table(test_fix$Class)
train_fix$Class <- ifelse(train$Class == "Eco", 1, ifelse(train$Class == "Eco Plus", 2, 3))
test_fix$Class <- ifelse(test$Class == "Eco", 1, ifelse(test$Class == "Eco Plus", 2, 3))

##randomForest 를 위한 as.factor
class(train_fix$target)
train_fix2 <- train_fix
train_fix2$target <- as.factor(train_fix$target)
class(train_fix2$target)

#기술통계 분석
summary(train)
summary(test)

windows()
par(mfrow=c(5, 5))
hist(train_fix$Gender); hist(train_fix$Customer.Type); hist(train_fix$Age); hist(train_fix$Type.of.Travel); hist(train_fix$Class)
hist(train_fix$Flight.Distance); hist(train_fix$Seat.comfort); hist(train_fix$Departure.Arrival.time.convenient); hist(train_fix$Food.and.drink); hist(train_fix$Gate.location)
hist(train_fix$Inflight.wifi.service); hist(train_fix$Inflight.entertainment); hist(train_fix$Online.support); hist(train_fix$Ease.of.Online.booking); hist(train_fix$On.board.service)
hist(train_fix$Leg.room.service); hist(train_fix$Baggage.handling); hist(train_fix$Checkin.service); hist(train_fix$Cleanliness); hist(train_fix$Online.boarding)
hist(train_fix$Departure.Delay.in.Minutes); hist(train_fix$Arrival.Delay.in.Minutes)
par(mfrow=c(1, 1))
dev.off()

#상관계수 확인하기
cor(train_fix)

#install.packages("corrplot")
library(corrplot)

windows()
corrplot(cor(train_fix))
dev.off()

#로지스틱 회귀모델
log_model <- glm(target ~ ., data = train_fix, family = 'binomial')
log_model

pred1 <- predict(log_model, newdata = test_fix, type = "response")
pred1

result_pred1 <- ifelse(pred1 >= 0.5, 1, 0)
result_pred1

table(result_pred1)

#랜덤포레스트
#install.packages('randomForest') #22.2.3 이후 4.1버전 이상만 지원가능한것 같음
library(randomForest)
ran_model <- randomForest(target ~ ., data = train_fix2)
ran_model

ran_model1 <- randomForest(target ~ ., data = train_fix2,
                           ntree = 800, mtry = 9)
ran_model1

ran_model2 <- randomForest(target ~ ., data = train_fix2,
                           ntree = 400, mtry = 5)
ran_model2

pred2 <- predict(ran_model, newdata = test_fix, type = "response")
pred2

table(pred2)

#데이터 전처리2

#####
##데이터 정규화Normalization(캐럿 패키지와 비교하니 잘못된 느낌)
#X1 = (X - Xmin) / (Xmax - Xmin)
#nor = function(x) {
#  result = (x - min(x)) / (max(x) - min(x))
#  return(result)
#}

#train_fix_nor <- nor(train_fix[1:22])
#train_fix_nor[,23] <- train_fix[23]
#train_fix2_nor <- nor(train_fix2[1:22])
#train_fix2_nor[,23] <- train_fix2[23]

##데이터 표준화Standardization(한줄은 가능)
#X1 = X - Xmean / Xsd
#sta = function(x) {
#  result = (x - mean(x)) / sd(x)
#  return(result)
#}

#train_fix_sta <- train_fix

#a <- sta(train_fix$Gender);a
#str(train_fix)

#for(i in 1:22){
#  train_fix_sta[i] <- sta(train_fix$colnames(train_fix[i]))
#}
#train_fix_sta <- sta(train_fix[1:22]) 에러남 
#####

##caret 패키지를 사용한 표준화, 정규화
#https://datadoctorblog.com/2021/01/23/R-Preprocessing-normalization/
#install.packages("caret")
library(caret)

#정규화
model_Nor = preProcess(x = train_fix[1:22], method = "range")
train_fix_nor2 <- predict(model_Nor, train_fix[1:22])
train_fix_nor2[,23] <- train_fix[23]

#####
table(train_fix_nor == train_fix_nor2)
head(train_fix_nor)
head(train_fix_nor2)
#####

train_fix2_nor2 <- train_fix_nor2
train_fix2_nor2[23] <- as.factor(train_fix$target)

model_Nor2 = preProcess(x = test_fix, method = "range")
test_fix_nor2 <- predict(model_Nor2, test_fix)

#표준화
model_Sta = preProcess(x = train_fix[1:22], method = c("center", "scale"))
train_fix_sta <- predict(model_Sta, train_fix[1:22])
train_fix_sta[,23] <- train_fix[23]

train_fix2_sta <- train_fix_sta
train_fix2_sta[23] <- as.factor(train_fix$target)

model_Sta2 = preProcess(x = test_fix, method = c("center", "scale"))
test_fix_sta <- predict(model_Sta2, test_fix)

#로지스틱 회귀모델(정규화, 표준화)
#정규화
log_model2 <- glm(target ~ ., data = train_fix_nor2, family = 'binomial')
log_model2

pred12 <- predict(log_model2, newdata = test_fix_nor2, type = "response")
pred12

result_pred12 <- ifelse(pred12 >= 0.5, 1, 0)
result_pred12

table(result_pred12)

#표준화
log_model3 <- glm(target ~ ., data = train_fix_sta, family = 'binomial')
log_model3

pred13 <- predict(log_model3, newdata = test_fix_sta, type = "response")
pred13

result_pred13 <- ifelse(pred13 >= 0.5, 1, 0)
result_pred13

table(result_pred12)

#랜덤포레스트(정규화, 표준화)
#정규화
ran_model2 <- randomForest(target ~ ., data = train_fix2_nor2)
ran_model2

pred22 <- predict(ran_model2, newdata = test_fix_nor2, type = "response")
pred22

table(pred22)

#표준화
ran_model3 <- randomForest(target ~ ., data = train_fix2_sta)
ran_model3

pred23 <- predict(ran_model3, newdata = test_fix_sta, type = "response")
pred23

table(pred23)
