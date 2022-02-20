#빅데이터A 2차 한호종
#####
#rm(list = ls())
#.rs.restartR()
#####

#데이터셋 로딩
train_air <- read.csv("C:/Rwork/airline_dataset/train.csv")

# id 제거
train_air <- train_air[,-1]

# 성별 변경
train_air$Gender[train_air$Gender == 'Female'] <- '0'
train_air$Gender[train_air$Gender == 'Male'] <- '1'

# 고객 타입 변경
train_air$Customer.Type[train_air$Customer.Type == 'disloyal Customer'] <- '0'
train_air$Customer.Type[train_air$Customer.Type == 'Loyal Customer'] <- '1'

train_air$Gender <-  as.numeric(train_air$Gender)
train_air$Customer.Type <-  as.numeric(train_air$Customer.Type)
train_air$target.fac <- as.factor(train_air$target)

#install.packages("caret")
library(caret)
model_Nor = preProcess(x = train_air, method = "range")
train_air <- predict(model_Nor, train_air)

#install.packages("dplyr")
library(dplyr)
train_air %>% filter(Type.of.Travel == 'Business travel') -> Business_tr
train_air %>% filter(Type.of.Travel == 'Personal Travel') -> Personal_tr

Business_tr %>% filter(Class == 'Eco') -> Bu_eco_tr
Business_tr %>% filter(Class == 'Business') -> Bu_bu_tr
Business_tr %>% filter(Class == 'Eco Plus') -> Bu_ep_tr

Personal_tr %>% filter(Class == 'Eco') -> Pe_eco_tr
Personal_tr %>% filter(Class == 'Business') -> Pe_bu_tr
Personal_tr %>% filter(Class == 'Eco Plus') -> Pe_ep_tr

#xgboost
#install.packages("xgboost")
library(xgboost)

#비지니스 / 이코노미
Bu_eco_mat <- as.matrix(Bu_eco_tr[-c(4, 5, 23, 24)])
train_lab <- Bu_eco_tr$target
xg_train <- xgb.DMatrix(data = Bu_eco_mat, label = train_lab) #xgb.DMatrix 객체변환

xgb_model1 <- xgboost(data = xg_train, max_depth = 2, eta =1, 
                     nthread = 4, nrounds = 4, objective = "multi:softmax",
                     num_class = 2, verbose = 0)

importance_matrix1 <- xgb.importance(colnames(Bu_eco_mat), model = xgb_model1)
importance_matrix1

xgb.plot.importance(importance_matrix1)

#비지니스 / 이코노미+
Bu_ep_mat <- as.matrix(Bu_ep_tr[-c(4, 5, 23, 24)])
train_lab <- Bu_ep_tr$target
xg_train <- xgb.DMatrix(data = Bu_ep_mat, label = train_lab) #xgb.DMatrix 객체변환

xgb_model2 <- xgboost(data = xg_train, max_depth = 2, eta =1, 
                     nthread = 4, nrounds = 4, objective = "multi:softmax",
                     num_class = 2, verbose = 0)

importance_matrix2 <- xgb.importance(colnames(Bu_ep_mat), model = xgb_model2)
importance_matrix2

xgb.plot.importance(importance_matrix2)

#비지니스 / 비지니스
Bu_bu_mat <- as.matrix(Bu_bu_tr[-c(4, 5, 23, 24)])
train_lab <- Bu_bu_tr$target
xg_train <- xgb.DMatrix(data = Bu_bu_mat, label = train_lab) #xgb.DMatrix 객체변환

xgb_model3 <- xgboost(data = xg_train, max_depth = 2, eta =1, 
                     nthread = 4, nrounds = 4, objective = "multi:softmax",
                     num_class = 2, verbose = 0)

importance_matrix3 <- xgb.importance(colnames(Bu_bu_mat), model = xgb_model3)
importance_matrix3

xgb.plot.importance(importance_matrix3)

#퍼스널 / 이코노미
Pe_eco_mat <- as.matrix(Pe_eco_tr[-c(4, 5, 23, 24)])
train_lab <- Pe_eco_tr$target
xg_train <- xgb.DMatrix(data = Pe_eco_mat, label = train_lab) #xgb.DMatrix 객체변환

xgb_model4 <- xgboost(data = xg_train, max_depth = 2, eta =1, 
                      nthread = 4, nrounds = 4, objective = "multi:softmax",
                      num_class = 2, verbose = 0)

importance_matrix4 <- xgb.importance(colnames(Pe_eco_mat), model = xgb_model4)
importance_matrix4

xgb.plot.importance(importance_matrix4)

#퍼스널 / 이코노미+
Pe_ep_mat <- as.matrix(Pe_ep_tr[-c(4, 5, 23, 24)])
train_lab <- Pe_ep_tr$target
xg_train <- xgb.DMatrix(data = Pe_ep_mat, label = train_lab) #xgb.DMatrix 객체변환

xgb_model5 <- xgboost(data = xg_train, max_depth = 2, eta =1, 
                      nthread = 4, nrounds = 4, objective = "multi:softmax",
                      num_class = 2, verbose = 0)

importance_matrix5 <- xgb.importance(colnames(Pe_ep_mat), model = xgb_model5)
importance_matrix5

xgb.plot.importance(importance_matrix5)

#퍼스널 / 이코노미
Pe_bu_mat <- as.matrix(Pe_bu_tr[-c(4, 5, 23, 24)])
train_lab <- Pe_bu_tr$target
xg_train <- xgb.DMatrix(data = Pe_bu_mat, label = train_lab) #xgb.DMatrix 객체변환

xgb_model6 <- xgboost(data = xg_train, max_depth = 2, eta =1, 
                      nthread = 4, nrounds = 4, objective = "multi:softmax",
                      num_class = 2, verbose = 0)

importance_matrix6 <- xgb.importance(colnames(Pe_bu_mat), model = xgb_model6)
importance_matrix6

xgb.plot.importance(importance_matrix6)
