## 2016年暑期课程设计
## 问题：Grupo Bimbo Inventory Demand
##  宾堡集团的库存需求
## 最大限度地提高销售和最大限度地减少烘焙食品的退回
## Daitu
## start:2016.06.21
## 参考借鉴kaggle上的公开程序
## 数据可视化，网络可视化分析



setwd("/Users/Daitu/数据分析/kaggle/Grupo Bimbo")
getwd()

## 加载包

library(data.table)
library(igraph)
library(network)
library(sna)
library(ndtv)
library(dplyr)

## 读取数据####
## 1:读取训练集
system.time({
  traindata <- fread("train.csv",sep=",",header = TRUE)
})
head(traindata)

## 2:读取客户名单数据
cliente_tabla <- fread("cliente_tabla.csv",sep=",",header = TRUE)
head(cliente_tabla)

## 3:读取产品名单数据
producto_tabla <- fread("producto_tabla.csv",sep=",",header = TRUE)
head(producto_tabla)

## 4:读取城镇和国家（州）数据
town_state <- fread("town_state.csv",sep=",",header = TRUE)
head(town_state)

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------


## 销售站和产品的关系网络图####
Agencia_products <- traindata %>%
  group_by(Agencia_ID,Producto_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Demanda_uni_equil = sum(Demanda_uni_equil),
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_rate = Return_Units/(Return_Units + Units)) %>%
  arrange(desc(Units)) %>%
  inner_join(producto_tabla,by="Producto_ID")

head(Agencia_products)
dim(Agencia_products)


graph.data.frame()
