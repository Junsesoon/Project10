rm(list = ls())

getwd()
setwd('C:\\Users\\You\\Desktop\\빅데이터\\빅데이터 수업자료\\R\\사례연구\\사례연구 11')

library(dplyr)


# 데이터
train_air <- read.csv('train.csv', header = T)
test_air <- read.csv('test.csv', header = T)

head(test_air)
str(train_air)



# id 제거
train_air <- train_air[,-1]

# 성별 변경
train_air$Gender[train_air$Gender == 'Female'] <- '1'
train_air$Gender[train_air$Gender == 'Male'] <- '2'

# 나이 10대, 20대 순으로 변경
train_air$Age <- train_air$Age * 0.1 %>% floor()  # 내림 10대 20대 30대 으로 변경

# 고객 타입 변경
train_air$Customer.Type[train_air$Customer.Type == 'disloyal Customer'] <- '1'
train_air$Customer.Type[train_air$Customer.Type == 'Loyal Customer'] <- '2'

# Type.of.Travel 변경
train_air$Type.of.Travel[train_air$Type.of.Travel == 'Business travel'] <- '1'
train_air$Type.of.Travel[train_air$Type.of.Travel == 'Personal Travel'] <- '2'

train_air$Gender <-  as.numeric(train_air$Gender)
train_air$Customer.Type <-  as.numeric(train_air$Customer.Type)
train_air$Type.of.Travel <- as.numeric(train_air$Type.of.Travel)
train_air$target <- as.factor(train_air$target)

str(train_air)


# id 제거
test_air <- test_air[,-1]

# 성별 변경
test_air$Gender[test_air$Gender == 'Female'] <- '1'
test_air$Gender[test_air$Gender == 'Male'] <- '2'

# 나이 10대, 20대 순으로 변경
test_air$Age <- test_air$Age * 0.1 %>% floor()  # 내림 10대 20대 30대로 변경

# 고객 타입 변경
test_air$Customer.Type[test_air$Customer.Type == 'disloyal Customer'] <- '1'
test_air$Customer.Type[test_air$Customer.Type == 'Loyal Customer'] <- '2'

# Type.of.Travel 변경
test_air$Type.of.Travel[test_air$Type.of.Travel == 'Business travel'] <- '1'
test_air$Type.of.Travel[test_air$Type.of.Travel == 'Personal Travel'] <- '2'

test_air$Gender <-  as.numeric(test_air$Gender)
test_air$Customer.Type <-  as.numeric(test_air$Customer.Type)
test_air$Type.of.Travel <- as.numeric(test_air$Type.of.Travel)

str(test_air)



# 훈련용 데이터셋
train_air %>% filter(Class == 'Eco') -> Eco_tr
train_air %>% filter(Class == 'Business') -> Business_tr
train_air %>% filter(Class == 'Eco Plus') -> EcoPlus_tr


# 검증용 데이터 셋
test_air %>% filter(Class == 'Eco') -> Eco_te
test_air %>% filter(Class == 'Business') -> Business_te
test_air %>% filter(Class == 'Eco Plus') -> EcoPlus_te




#####
# 정규화
a1 <- train_air[,-c(1,2,3,4,5)]
b1 <- test_air[,-c(1,2,3,4,5)]
nor_minmax = function(x){
  result = (x - min(x)) / (max(x) - min(x))
  return(result)
}

a1 <- sapply(a1[,c(2:19)], nor_minmax)
a1 <- data.frame(a1)
a1 <- cbind(train_air[,6],a1)
a1$target <- as.factor(a1$target)
names(a1)[1] <- 'Class'
str(a1)

b1 <- sapply(b1[,c(2:18)], nor_minmax)
b1 <- data.frame(b1)
b1 <- cbind(test_air[,6], b1)
names(b1)[1] <- 'Class'
b1
str(b1)
#####


# 랜덤포레스트 Eco
library(randomForest)
Eco_rf <- randomForest(target ~ .,
                              data=Eco_tr,
                              na.action = na.omit,
                              ntree=100,
                              proximity=T,
                              importance=T)
Eco_rf

importance(Eco_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Eco_rf)  # 

Eco_pred <- predict(Eco_rf, Eco_te)

table(Eco_pred) # = Eco_te$target
table(Eco_tr$target)


# 랜덤포레스트 Business
Business_rf <- randomForest(target ~ .,
                       data=Business_tr,
                       na.action = na.omit,
                       ntree=100,
                       proximity=T,
                       importance=T)
Business_rf

importance(Business_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Business_rf)  

Business_pred <- predict(Business_rf, Business_te)

table(Business_pred) # = Business_te$target
table(Business_tr$target)

# 랜덤포레스트 Eco Plus
EP_rf <- randomForest(target ~ .,
                       data=EcoPlus_tr,
                       na.action = na.omit,
                       ntree=100,
                       proximity=T,
                       importance=T)
EP_rf

importance(EP_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(EP_rf)  # 

EP_pred <- predict(EP_rf, EcoPlus_te)

table(EP_pred) # = EcoPlus_te$target
table(EcoPlus_tr$target)

