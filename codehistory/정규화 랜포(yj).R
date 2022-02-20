rm(list = ls())

getwd()
setwd('C:\\Users\\You\\Desktop\\빅데이터\\빅데이터 수업자료\\R\\사례연구\\사례연구 11')

library(dplyr)
library(caret)

# 데이터
train_air <- read.csv('train.csv')

head(train_air)
str(train_air)

# id 제거
train_air <- train_air[,-c(1,2)]

# 고객 타입 변경
train_air$Customer.Type[train_air$Customer.Type == 'disloyal Customer'] <- '0'
train_air$Customer.Type[train_air$Customer.Type == 'Loyal Customer'] <- '1'

# Type.of.Travel 변경
train_air$Type.of.Travel[train_air$Type.of.Travel == 'Business travel'] <- '0'
train_air$Type.of.Travel[train_air$Type.of.Travel == 'Personal Travel'] <- '1'

train_air$Customer.Type <-  as.numeric(train_air$Customer.Type)
train_air$Type.of.Travel <- as.numeric(train_air$Type.of.Travel)
train_air$target <- as.factor(train_air$target)


# 정규화
model_Nor = preProcess(x = train_air, method = "range")
train_air <- predict(model_Nor, train_air)

# 여행 타입별 분류
train_air %>% filter(Type.of.Travel == '0') -> Business_tr
train_air %>% filter(Type.of.Travel == '1') -> Personal_tr


# 클래스별 분류
Business_tr %>% filter(Class == 'Eco') -> Bu_eco_tr
Business_tr %>% filter(Class == 'Business') -> Bu_bu_tr
Business_tr %>% filter(Class == 'Eco Plus') -> Bu_ep_tr

Bu_eco_tr <- Bu_eco_tr[,-c(1,3,4)]
Bu_bu_tr <- Bu_bu_tr[,-c(1,3,4)]
Bu_ep_tr <- Bu_ep_tr[,-c(1,3,4)]

Personal_tr %>% filter(Class == 'Eco') -> Pe_eco_tr
Personal_tr %>% filter(Class == 'Business') -> Pe_bu_tr
Personal_tr %>% filter(Class == 'Eco Plus') -> Pe_ep_tr

Pe_eco_tr <- Pe_eco_tr[,-c(1,3,4)]
Pe_bu_tr <- Pe_bu_tr[,-c(1,3,4)]
Pe_ep_tr <- Pe_ep_tr[,-c(1,3,4)]

length(Business_tr$Customer.Type)
length(Bu_eco_tr$Gender)
length(Bu_bu_tr$Gender)
length(Bu_ep_tr$Gender)

length(Personal_tr$Gender)
length(Pe_eco_tr$Gender)
length(Pe_bu_tr$Gender)
length(Pe_ep_tr$Gender)



# 랜포 - 타입 : 비즈니스

# 비즈니스 - 에코
library(randomForest)
Bu_eco_rf <- randomForest(target ~ .,
                       data=Bu_eco_tr,
                       na.action = na.omit,
                       ntree=100,
                       proximity=T)
Bu_eco_rf

importance(Bu_eco_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Bu_eco_rf)  




# 비즈니스 - 비즈니스
Bu_bu_rf <- randomForest(target ~ .,
                            data=Bu_bu_tr,
                            na.action = na.omit,
                            ntree=100,
                            proximity=T)
Bu_bu_rf

importance(Bu_bu_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Bu_bu_rf)  


# 비즈니스 - 에플
Bu_ep_rf <- randomForest(target ~ .,
                      data=Bu_ep_tr,
                      na.action = na.omit,
                      ntree=100,
                      proximity=T)
Bu_ep_rf

importance(Bu_ep_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Bu_ep_rf)  # 






# 랜포 - 타입 : 퍼스널
# 퍼스널 - 에코
Pe_eco_rf <- randomForest(target ~ .,
                          data=Pe_eco_tr,
                          na.action = na.omit,
                          ntree=100,
                          proximity=T)
Pe_eco_rf


importance(Pe_eco_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Pe_eco_rf)  


# 퍼스널 - 비즈니스
Pe_bu_rf <- randomForest(target ~ .,
                          data=Pe_bu_tr,
                          na.action = na.omit,
                          ntree=100,
                          proximity=T)
Pe_bu_rf

importance(Pe_bu_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Pe_bu_rf)  


# 퍼스널 - 에플
Pe_ep_rf <- randomForest(target ~ .,
                          data=Pe_ep_tr,
                          na.action = na.omit,
                          ntree=100,
                          proximity=T)
Pe_ep_rf

importance(Pe_ep_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Pe_ep_rf)  

