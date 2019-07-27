## 2016年暑期课程设计
## 问题：Grupo Bimbo Inventory Demand
##  宾堡集团的库存需求
## 最大限度地提高销售和最大限度地减少烘焙食品的退回
## Daitu
## start:2016.06.21
## 参考借鉴kaggle上的公开程序
## 使用随机森林回归进行预测


setwd("/Users/Daitu/数据分析/kaggle/Grupo Bimbo")
getwd()

## 加载包

library(data.table)
library(dplyr)
library(randomForest)
library(doParallel)

## 读取数据####
## 1:读取训练集
colClasses <- c("integer","character","character","character","character","character",
                "numeric","numeric","numeric","numeric","numeric")
system.time({
  traindata <- fread("train.csv",sep=",",header = TRUE,colClasses=colClasses)
})
head(traindata)
str(traindata)


## 2:读取测试集
tcolClasses <- c("integer","integer","character","character","character","character","character")
system.time({
  testdata <- fread("test.csv",sep=",",header = TRUE,colClasses=tcolClasses)
})
head(testdata)
str(testdata)
##数据的准备和变换####
products <- traindata %>%
  group_by(Producto_ID,Canal_ID,Agencia_ID,Ruta_SAK,Ruta_SAK,Semana) %>%
  summarise(Demanda_uni_equil = mean(Demanda_uni_equil))
head(products)

## 训练机和测试集的划分

train <- as.data.frame( products[(products$Semana %in% c(3:7)),])
class(train)
train_x <- train[,1:4]
train_y <- train[,6]
test <- as.data.frame(products[products$Semana %in% c(8,9),])
test_x <- test[,1:4]
test_y <- test[,6]

## 随机森林进行预测####
# ## 并行运算
# system.time({
#   cl <- makeCluster(4)
#   registerDoParallel(cl)
#   RFgre <- foreach(ntree = rep(50,4),
#                    .multicombine = TRUE,
#                    .packages = "randomForest") %dopar%
#     randomForest(x = train_x,y = train_y,xtest = test_x,ytest = test_y,ntree = ntree)
#   stopCluster(cl)
# })

library(h2o)

h2o.randomForest()





