## 2016年暑期课程设计
## 问题：Grupo Bimbo Inventory Demand
##  宾堡集团的库存需求
## 最大限度地提高销售和最大限度地减少烘焙食品的退回
## Daitu
## start:2016.06.20


setwd("/Users/Daitu/数据分析/kaggle/Grupo Bimbo")
getwd()

## 加载包

library(data.table)
library(ggplot2)


## 读取数据####
## 1:读取训练集
# system.time({
#   traindata <- read.csv("train.csv",header = TRUE)
# })
# 用户    系统    流逝 
# 415.471  56.556 486.345 
system.time({
  traindata <- fread("train.csv",sep=",",header = TRUE)
})
# Read 74180464 rows and 11 (of 11) columns from 2.980 GB file in 00:00:29
# 用户   系统   流逝 
# 26.553  2.077 28.869
head(traindata,5)
str(traindata)

## 2:读取客户名单数据
cliente_tabla <- fread("cliente_tabla.csv",sep=",",header = TRUE)

## 3:读取产品名单数据
producto_tabla <- fread("producto_tabla.csv",sep=",",header = TRUE)

## 4:读取城镇和国家（州）数据
town_state <- fread("town_state.csv",sep=",",header = TRUE)
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------



## 数据的描述统计####
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## 1:分析数据的周数：Semana
Semana <- data.frame(table(traindata$Semana))
colnames(Semana) <- c("Semana","Freq")
# 3        4        5        6        7        8        9 
# 11165207 11009593 10615397 10191837 10382849 10406868 10408713 
## 条形图
ggplot(data = Semana,aes(Semana,Freq)) + 
  geom_bar(stat = "identity", width = 0.6,fill = "lightblue",colour = "black") + 
  theme_grey(base_family = "STKaiti") +
  scale_y_continuous() + 
  labs(x="周数",y="数据条数",title = "每周数据的记录数")



## 分析销售站情况：Agencia_ID
Agencia <- data.frame(table(traindata$Agencia_ID))
colnames(Agencia) <- c("Agencia_ID","Freq")
dim(Agencia)  # 552个销售站
Agencia <- Agencia[order(Agencia$Freq,decreasing = TRUE),]
summary(Agencia$Freq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 31    8669   44920  134400  230600  806300 
## 查看客户名是否和客户名数据相同
length(unique(town_state$Agencia_ID))
# 训练集中的销售站全在town_state数据集中，反之不成立，说明有些销售站并没有卖这个厂的产品
sum(Agencia$Agencia_ID %in% town_state$Agencia_ID) 

length(unique(town_state$Town))   # 260个镇

length(unique(town_state$State))   # 33个州

## 将有该公司产品卖的销售站的数据提取出来
index <- which(town_state$Agencia_ID %in% Agencia$Agencia_ID)
town_state_use <- town_state[index,]
## 产看不同州的销售站的数量分布
ggplot(town_state_use,aes(State)) + 
  geom_bar(stat = "count",width = 0.7,fill = "cyan",colour = "black") + 
  theme_grey(base_family = "STKaiti") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y="销售站数量",title = "各州的销售站分布")



## 训练集中，每个销售站的数据记录条数
ggplot(data = Agencia,aes(reorder(Agencia_ID,Freq),Freq)) + 
  geom_bar(stat = "identity",fill = "lightblue",colour = "cyan") + 
  theme_grey(base_family = "STKaiti") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="销售站",y="数据条数",title = "销售站的记录数")


## 5:分析销售渠道 Canal_ID
Canal <- data.frame(table(traindata$Canal_ID))
colnames(Canal) <- c("Canal_ID","Freq")
# 1        2        4        5        6        7        8        9       11 
# 67435217   839496  3757773   145818   281389   671128    66970      378   982295 
## 查看不同销售渠道的数据条数
ggplot(Canal,aes(Canal_ID,Freq)) + 
  geom_bar(stat = "identity",width = 0.7,fill = "cyan",colour = "black") + 
  theme_grey(base_family = "STKaiti") + 
  labs(x="销售渠道",y = "记录条数",title = "不同销售渠道的数据条数")

## 6:分析路线的情况
Ruto_SAK <- data.frame(table(traindata$Ruta_SAK))
colnames(Ruto_SAK) <- c("Ruto_sak","Freq")
dim(Ruto_SAK)   # 共有3602条记录
# 记录数大于500的路线有1905条
dim(Ruto_SAK[Ruto_SAK$Freq > 500,])

# 记录数大于10000的路线有956条
dim(Ruto_SAK[Ruto_SAK$Freq > 10000,])

## 7:分析客户Id的情况
cliente <- data.frame(table(traindata$Cliente_ID))
colnames(cliente) <- c("cliente_id","Freq")
dim(cliente)
summary(cliente$Freq)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.00     20.00     55.00     84.24    120.00 124100.00 
# 记录数大于5000的客户有2条
dim(cliente[cliente$Freq > 5000,])

# 记录数小于 10的客户有117708 条
dim(cliente[cliente$Freq < 10,])

## 可视化查看，折线图

boxplot(cliente$Freq)


## 8:分析产品Id的情况
Producto <- data.frame(table(traindata$Producto_ID))
colnames(Producto) <- c("Producto_id","Freq")
dim(Producto)  # 1799 种产品
summary(Producto$Freq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1      34     582   41230    6708 2147000 

# 记录数大于5000的客户有495条
dim(Producto[Producto$Freq > 5000,])

# 记录数小于 10的客户有271  条
dim(Producto[Producto$Freq < 10,])

## 可视化查看，折线图

boxplot(Producto$Freq)


## 9:分析本周的销售量
Venta_uni_hoy <- traindata$Venta_uni_hoy
summary(Venta_uni_hoy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    2.00    3.00    7.31    7.00 7200.00 


hist(traindata$Venta_uni_hoy,breaks = 500)


# plot(traindata$Venta_uni_hoy,type = "l")











