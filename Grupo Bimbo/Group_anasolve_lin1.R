## 2016年暑期课程设计
## 问题：Grupo Bimbo Inventory Demand
##  宾堡集团的库存需求
## 最大限度地提高销售和最大限度地减少烘焙食品的退回
## Daitu
## start:2016.06.23
## 参考借鉴kaggle上的公开程序
## 对各种方法预测得到的结果进行分析

setwd("/Users/Daitu/数据分析/kaggle/Grupo Bimbo")
getwd()

## 加载包

library(data.table)
library(ggplot2)
library(dplyr)
library(treemap)

## 读取数据####
## 1:读取训练集
system.time({
  traindata <- fread("train.csv",sep=",",header = TRUE)
})
head(traindata)


system.time({
  testdata <- fread("test.csv",sep=",",header = TRUE)
})
head(testdata)

## 1:对通过GBM预测得到的结果进行分析####
## 读取预测数据，ntree ＝ 25
h2o_gbm <- fread("h2o_gbmother.csv",sep = ",",header = TRUE)
# 对 需求量数据向上取整
h2o_gbm$Demanda_uni_equil <- ceiling(h2o_gbm$Demanda_uni_equil)
head(h2o_gbm)
## 将预测数据与测试数据组合
testdata$Demanda_uni_equil <- h2o_gbm$Demanda_uni_equil
## 将数据根据星期分组
trainsemana <- traindata %>%
  group_by(Semana,Canal_ID) %>%
  summarise(Demanda_uni_equil = sum(Demanda_uni_equil))

testsemana <- testdata %>%
  group_by(Semana,Canal_ID) %>%
  summarise(Demanda_uni_equil = sum(Demanda_uni_equil))

semana_canal <- rbind(trainsemana,testsemana)
semana_canal$Semana <- as.factor(semana_canal$Semana)
## 可视化查每周的需求量
ggplot(data = semana_canal,aes(x = Semana,y = Demanda_uni_equil)) +
  geom_bar(stat = "identity", fill = "red",color = "black",alpha = 0.5) +
  theme_bw(base_family = "STKaiti") +
  facet_wrap(~Canal_ID) +
  scale_x_discrete() + 
  labs(x = "星期",y = "需求量",title = "销售渠道")

## 没有办法放在一块对比，因为测试集中的预测数据可能不是所有的数据








