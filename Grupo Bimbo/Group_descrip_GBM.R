## 2016年暑期课程设计####
## 问题：Grupo Bimbo Inventory Demand
##  宾堡集团的库存需求
## 最大限度地提高销售和最大限度地减少烘焙食品的退回
## Daitu
## start:2016.06.22
## 参考借鉴kaggle上的公开程序
## 使用梯度提升机进行预测

##设置工作文件夹
setwd("/Users/Daitu/数据分析/kaggle/Grupo Bimbo")
getwd()

library(ggplot2)

## 设置集群 ####
print(paste("Set up Cluster",Sys.time()))
library(h2o) # R API is just a library
## 启动一个集群; 定一位4核同时计算；
h2o.init(nthreads=4,max_mem_size='12G')  

## 加载数据#### 

print(paste("加载数据",Sys.time()))
## 读取整个训练数据，使用所有的核
system.time({
  train<-h2o.uploadFile("train.csv",destination_frame = "train.hex")
})

train[1:5,] ## 查看训练集的前几行
## 将训练集的(预测目标+1)取对数
train$target<-log(train$Demanda_uni_equil+1)
train[1:5,]

h2o.median(train$target)

## 数据分区
print(paste("数据分区",Sys.time()))

## 这个模型将会把数据分为3个部分，根据星期数据进行分区：
##   one to generate product averages, a second to fit a model, and a third to evaluate the model
## 第一个数据用来生成产品均值，第二部分数据用来拟合一个模型，第三部分数据用来计算模型
dev<-train[train$Semana <= 5,]                    ## gets Semana 3,4,5
val<-train[train$Semana > 5 & train$Semana <= 8,]  ## gets Semana 6, 7,8
val[1:5,]
final<-train[train$Semana == 9,]                  ## gets Semana 9
final[1:5,]

## 模型：产品分组&GBM####

print(paste("Model: Product Groups & GBM",Sys.time()))

## 使用测试集中用来预测的字段变量进行预测，剔除ID和星期，
predictors<-c("Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID")

## first part of model: use product averages, created on the dev set
##  this is the only time we will use the dev set
## 模型的第一部分：使用产品的均值，在dev数据集上创建
## 这是dev数据集的唯一的一次使用
groups<-h2o.group_by(data=dev,by="Producto_ID",mean("target"))
groups[1:5,]
h2o.median(groups$mean_target)

## apply groups back into dev and validation data sets as "mean_target"
## if there are NAs for this (new products), use a constant; used median of entire train target
## 使用分组后的数据集dev，生成新的确认数据（val）
## 如果数据集中有NAS（代表新的产品），使用中位数进行代替。
newVal<-h2o.merge(x=val,y=groups,all.x = T)
newVal[1:5,]
newVal$mean_target[is.na(newVal$mean_target)]<-h2o.median(train$target)
newVal[1:5,]
newFinal<-h2o.merge(x=final,y=groups,all.x = T)
newFinal[1:5,]
newFinal$mean_target[is.na(newFinal$mean_target)]<-h2o.median(train$target)
newFinal[1:5,]


## 训练 GBM; 使用参数以保持整体运行时间在20分钟内
## this model is fit on Semana 6 & 7 & 8, and evaluated on Semana 9.
g<-h2o.gbm(
  training_frame = newVal,      ## H2O frame holding the training data
  validation_frame = newFinal,  ## extra holdout piece for three layer modeling
  x=predictors,                 ## 建立模型的预测变量
  y="target",                   ## target: using the logged variable created earlier
  model_id="gbm1",              ## internal H2O name for model
  distribution = "poisson",     ## 目标数据服从poisson分布
  ntrees = 50,                  ## 使用50棵树建立模型
  learn_rate = 0.3,             ## lower learn_rate is better, but use high rate to offset few trees
  score_tree_interval = 3,      ## score every 3 trees
  sample_rate = 0.5,            ## use half the rows each scoring round
  col_sample_rate = 0.8,        ## use 4/5 the columns to decide each split decision
  offset_column = "mean_target"
)

## 查看模型
summary(g)

# 删除不再需要的较大的数据集
# h2o.rm(train)
# h2o.rm(dev)
# h2o.rm(val)
# h2o.rm(newVal)


## 进行预测#####

print(paste("Create Predictions",Sys.time()))
## 加载测试集
test<-h2o.uploadFile("test.csv",destination_frame = "test.hex")
test[1:5,] ## 查看测试集的前几行数据
## merge in the offset column, just as with val and final
newTest<-h2o.merge(x=test,y=groups,all.x = T)
newTest[1:5,]
newTest$mean_target[is.na(newTest$mean_target)]<-h2o.median(train$target)
newTest[1:5,]
p<-h2o.predict(g,newTest)
p<-exp(p)-1
summary(p)


## 创建提交文件#####

print(paste("Create Submission",Sys.time()))
submissionFrame<-h2o.cbind(test$id,p)
colnames(submissionFrame)<-c("id","Demanda_uni_equil")
h2o.exportFile(submissionFrame,path="h2o_gbm50.csv")  ## 输出文件










