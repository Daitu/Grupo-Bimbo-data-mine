## 2016年暑期课程设计
## 问题：Grupo Bimbo Inventory Demand
##  宾堡集团的库存需求
## 最大限度地提高销售和最大限度地减少烘焙食品的退回
## Daitu
## start:2016.06.21
## 参考借鉴kaggle上的公开程序


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
# Read 74180464 rows and 11 (of 11) columns from 2.980 GB file in 00:00:29
# 用户   系统   流逝 
# 26.553  2.077 28.869

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



## 销售站的数据分析#####
## 2 ：销售站Agencia 和 州State
agencias <- traindata %>%
  group_by(Agencia_ID) %>%         # 数据按照销售战进行分组统计
  summarise(Units = sum(Venta_uni_hoy), # 总结多个值为一个值，units：本销售站的销量和
            Pesos = sum(Venta_hoy),  # 本周的销售量（比索）之和
            Return_Units = sum(Dev_uni_proxima),  # 下星期的返回量之和
            Return_Pesos = sum(Dev_proxima),  # 下星期的返回量（比索）之和
            Net = sum(Demanda_uni_equil)) %>%   # 调整后的需求和
  mutate(Net_Pesos = Pesos - Return_Pesos,  # mutate:添加新的变量 
         Return_Rate = Return_Units / (Units+Return_Units)) %>%  # 添加变量退货比率
  arrange(desc(Units)) %>%     # 将数据按照变量Units的降序排列
  inner_join(town_state, by="Agencia_ID")   # 按照变量Agencia_ID，连接两个表，return all rows from x 

## 可视化x：每天销量，y：销售站的数量
ggplot(agencias, aes(x=Units/7))+
  geom_histogram(fill="red", color="gray", binwidth=10000)+ #条形图的宽度为10000
  theme_bw(base_family = "STKaiti") + 
  scale_x_continuous(labels=function(x)paste(x/1000, "k"))+
  labs(x = "每天的平均销量",y = "销售站数量",title = "销售站的销量")

## 前100的销售站的销量树图
treemap(agencias[1:100, ], 
        index=c("Agencia_ID"), vSize="Units", vColor="Return_Rate", 
        type="value", title.legend="Units return %", title="Top 100 agencias")

## 销售站的历史数据分析
agencias_history <- traindata %>%
  group_by(Agencia_ID, Semana) %>%  # 数据按照销售站、星期数，进行分组统计
  summarise(Units = sum(Venta_uni_hoy),  # 总结多个值为一个值，units：销量和
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(Agencia_ID, Semana) %>% # 将数据按照变量 销售站、星期数的降序排列
  inner_join(town_state, by="Agencia_ID")
## 取出销售量前30的销售站ID
top30agencias <- agencias$Agencia_ID[1:30]

## 销量前30的销售站每周的销量和退货率图
ggplot(agencias_history %>% filter(Agencia_ID %in% top30agencias))+
  geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
  theme_bw(base_family = "STKaiti") +
  facet_wrap(~Agencia_ID)+   # 按照销售站划分成子图
  scale_y_continuous(labels=function(x)paste(x/1000, "k"))+
  scale_fill_gradient(name="退回\n百分比", low="white", high="red")+
  ggtitle("销量前30的销售站") + ylab("销量") +xlab("星期")

## 每个州的销售数据的分析
states <- agencias_history %>%
  group_by(State, Semana) %>%     #数据按照州和星期分组
  summarise(Units = sum(Units),
            Pesos = sum(Pesos),
            Return_Units = sum(Return_Units),
            Return_Pesos = sum(Return_Pesos),
            Net = sum(Net)) %>%
  mutate(Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))   # 数据按照销量排列

## 地点－－星期 －－退回百分比 图像可视化
ggplot(states)+
  geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
  theme_bw(base_family = "STKaiti") +
  facet_wrap(~State)+
  scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
  scale_fill_gradient(name="退回\n百分比", low="white", high="red")+
  ggtitle("州的销售量")+ ylab("销量") +xlab("星期")


## 销售渠道的分析####

canals <- traindata %>%
  group_by(Canal_ID, Semana) %>%  #根据销售渠道和星期进行分组
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))
# 销售渠道1占据主要的销量
treemap(canals, index=c("Canal_ID"), vSize="Units", type="index", 
        title="Canals repartition")

## 销售渠道的销量和星期和退货率
ggplot(canals)+
  geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
  theme_bw(base_family = "STKaiti") +
  facet_wrap(~Canal_ID, scale="free")+
  scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
  scale_fill_gradient(name="退回\n百分比", low="white", high="red")+
  ggtitle("销售渠道")+ ylab("销量") +xlab("星期")

## 销售渠道和销售站分析####
agencias_canals <- traindata %>%
  group_by(Agencia_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID)) #添加该销售渠道有多少销售站

## 销售渠道有多少销售站可视化
ggplot(agencias_canals)+
  geom_histogram(aes(x=n_canals), fill="red", color="black", alpha="0.5", binwidth=0.5)+
  theme_bw(base_family = "STKaiti") +
  scale_x_continuous(breaks=1:5)+
  scale_y_continuous()+
  theme(axis.text.x=element_text(hjust=1)) +
  labs(x = "销售渠道数量",y = "销售站数量",title = "销售站的销售渠道量")
# 大部分销售站有1条销售渠道，只有很少的销售站有超过三条的销售渠道

## 销售路线的分析####
routes <- traindata %>% group_by(Ruta_SAK) %>%
  summarise(n_Agencias = n_distinct(Agencia_ID), #销售路线有多少销售站
            n_Clients = n_distinct(Cliente_ID), #销售路线有多少客户
            Units=sum(Venta_uni_hoy),  #销售路线的销售量
            Return_Units = sum(Dev_uni_proxima)) %>%  #销售路线的销售量退货量
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>% # 添加退货率变量
  arrange(desc(Units))   # 按照销量排序

ggplot(routes, aes(x=Units/7))+
  geom_histogram(fill="red", color="gray", binwidth=5000)+
  theme_bw(base_family = "STKaiti") +
  scale_x_continuous(labels=function(x)paste(x/1000, "k"))+
  scale_y_continuous()+
  labs(x = "平均每天销量",y = "销售路线数量")
## 大部分的销售路线的销售量并不多，超过2/3的销售路线每天的销售量不超过10千


## 销售路线和销售站#####
routes_agencias <- traindata %>% group_by(Ruta_SAK, Agencia_ID) %>%
  summarise(count=n(),      #当前分组的观测数
            n_Clients = n_distinct(Cliente_ID), # 客户数量
            Units=sum(Venta_uni_hoy),  #销售量求和
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))
top100routes <- routes$Ruta_SAK[1:100]  # 销量前100的路线
top100agencias <- agencias$Agencia_ID[1:100] # 销量前100的销售站
## 可视化
ggplot(routes_agencias %>% 
         filter(Ruta_SAK %in% top100routes, Agencia_ID %in% top100agencias))+
  geom_point(aes(x=as.character(Ruta_SAK), 
                 y=as.character(Agencia_ID), 
                 size=Units, color=Return_Rate))+
  theme_bw(base_family = "STKaiti") +
  scale_color_gradient(name="退回\n百分比", low="blue", high="red")+
  scale_size_continuous(name = "销量",range = c(1,4)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "销售路线",y = "销售站",title = "销量前100的销售站&销售路线")

top50routes <- routes$Ruta_SAK[1:50]  # 销量前100的路线
top50agencias <- agencias$Agencia_ID[1:50] # 销量前100的销售站
## 可视化
ggplot(routes_agencias %>% 
         filter(Ruta_SAK %in% top50routes, Agencia_ID %in% top50agencias))+
  geom_point(aes(x=as.character(Ruta_SAK), 
                 y=as.character(Agencia_ID), 
                 size=Units, color=Return_Rate))+
  theme_bw(base_family = "STKaiti") +
  scale_color_gradient(name="退回\n百分比", low="blue", high="red")+
  scale_size_continuous(name = "销量",range = c(1,6)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(x = "销售路线",y = "销售站",title = "销量前50的销售站&销售路线")

## 对客户数据进行分析#####
sales <- traindata %>%     #客户数据
  group_by(Cliente_ID) %>%   # 按照客户id分组
  summarise(Units = sum(Venta_uni_hoy), 
            Pesos = sum(Venta_hoy),  # 本周销售金额
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),  # 下星期的退回金额
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units),
         Avg_Pesos = Pesos / Units) %>%    # 单价
  mutate(Net_Pesos = Pesos - Return_Pesos) %>%  # 实际销售金额
  inner_join(cliente_tabla, by="Cliente_ID") %>%
  arrange(desc(Pesos))  # 本周销售金额排序
# 花费量前100个客户的树形图
# 可见有一个大客户：Puebla Remision
treemap(sales[1:100, ], 
        index=c("NombreCliente"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 clients")

## 客户的累积消耗量
sales$Cum_Units <- cumsum(sales$Units) / sum(sales$Units)  # 累积百分比
s <- seq(1, 800000, 100)   # 约有80万个客户
ggplot()+geom_line(aes(x=s, y=sales$Cum_Units[s]))+
  theme_bw(base_family = "STKaiti") +
  scale_x_continuous(labels=function(x) paste(x/1000, "k"))+
  ggtitle("客户分配")+ xlab("客户数量")+ylab("累积百分比(销量)")
## 前20万客户约贡献了75％的销售量



## 客户和销售站分析####
agencias_by_client <- traindata %>%
  group_by(Cliente_ID) %>%  #按照客户id分组
  summarise(n_agencias = n_distinct(Agencia_ID)) %>% #多少个销售站
  inner_join(cliente_tabla, by="Cliente_ID") 

table(agencias_by_client$n_agencias)

# 1      2      3      4      5      9     62 
# 844113  37510   3771     19      1      1      1 
# 大部分的客户只从一个销售站购买，只有几个客户购买狗的销售站 >= 5

agencias_by_client %>% filter(n_agencias %in% c(5, 9, 62))  #返回符合条件的行
# Cliente_ID n_agencias                      NombreCliente
# (int)      (int)                              (chr)
# 1     188391          9                DESAYUNOS ESCOLARES
# 2     653378         62                    PUEBLA REMISION
# 3    1274327          5 COMERCIALIZADORA LA PUERTA DEL SOL

##  客户和购买渠道分析#####
clients_canals <- traindata %>%
  group_by(Cliente_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID))

## 大多数客户只有一个购买渠道。不同的销售渠道可以为一个客户提供服务。
table(clients_canals$n_canals)
# 1      2      3      4 
# 874022   6516     65      1 

# 很少有销售站有同一个客户通过多个渠道。
clients_agencies_canals <- traindata %>%
  group_by(Cliente_ID, Agencia_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID))

table(clients_agencies_canals$n_canals)


## 客户和路线分析#####
clients_routes <- traindata %>%
  group_by(Cliente_ID) %>%
  summarise(n_routes = n_distinct(Ruta_SAK))

## 大多数客户只有不到5个仓库的交货，但超过240个客户的工作与10个仓库或更多。
sum(clients_routes$n_routes >= 10)
ggplot(clients_routes)+
  geom_histogram(aes(x=n_routes), fill="red", color="black", alpha="0.5", binwidth=1)+
  theme_bw(base_family = "STKaiti") +
  scale_y_continuous(labels=function(x) paste(x/1000, "k"))+
  ggtitle("客户和销售路线")+ xlab("路线数量")+ylab("客户数量")


## 对集团销售的产品进行分析#####

products <- traindata %>% group_by(Producto_ID) %>%  #根据生产的产品进行分组
  summarise(Units = sum(Venta_uni_hoy),  # 销量
            Pesos = sum(Venta_hoy),    # 卖出的总钱数
            Return_Units = sum(Dev_uni_proxima),  # 被退回的总量
            Return_Pesos = sum(Dev_proxima),  #备退回的总钱数
            Net = sum(Demanda_uni_equil)) %>%  #调整后的我总需求
  mutate(Avg_Pesos = Pesos / Units,  # 每种产品的单价
         Return_Rate = Return_Units / (Units+Return_Units)) %>%  # 退货率
  filter(!is.nan(Avg_Pesos)) %>% #剔除没有单价的商品
  inner_join(producto_tabla, by="Producto_ID") %>%
  arrange(desc(Units))

products$NombreProducto <- factor(as.character(products$NombreProducto), levels=products$NombreProducto)

# 销量前100的产品树图
treemap(products[1:100, ], 
        index=c("NombreProducto"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 products")

## 产品的家的密度分布
ggplot(products, aes(x=Avg_Pesos))+
  geom_histogram(aes(y=..density..), fill="gray", color="black", alpha="0.8")+
  geom_density(fill="red", alpha="0.3")+
  theme_bw(base_family = "STKaiti") +
  scale_x_continuous(lim=c(0, 50))+
  ggtitle("产品单价的分布")+ xlab("平均单价")+ylab("密度")

## 产品和销售站
products_agencies <- traindata %>% group_by(Agencia_ID) %>%
  summarise(n_products = n_distinct(Producto_ID))

## 大多数销售站会卖100～200种产品
ggplot(products_agencies)+
  geom_histogram(aes(x = n_products), fill="red", color="black", alpha="0.5", binwidth=10)+
  theme_bw(base_family = "STKaiti") +
  ggtitle("销售站出售的产品数量")+ xlab("产品数量(种)")+ylab("销售站数量(个)")

## 产品和销售路线
routes_products <- traindata %>% group_by(Producto_ID) %>%
  summarise(n_routes = n_distinct(Ruta_SAK))
## 大部分的产品只有几条销售路线，只有几种产品的销售路线很多
ggplot(routes_products)+
  geom_histogram(aes(x=n_routes), fill="red", color="black", alpha="0.5", binwidth=10)+
  theme_bw(base_family = "STKaiti") +
  ggtitle("产品和销售线路")+ xlab("销售线路(条)")+ylab("产品数量(种)")


## 产品和销售路线 top100
routes.products <- traindata %>% group_by(Ruta_SAK, Producto_ID) %>%
  summarise(count=n(),
            n_Agencias = n_distinct(Agencia_ID),
            n_Clients = n_distinct(Cliente_ID),
            Units=sum(Venta_uni_hoy),
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))


top100routes <- routes$Ruta_SAK[1:100]
top100products <- products$Producto_ID[1:100]

ggplot(routes.products %>% 
         filter(Ruta_SAK %in% top100routes, Producto_ID %in% top100products))+
  geom_point(aes(x=as.character(Ruta_SAK), 
                 y=as.character(Producto_ID), 
                 size=Units, color=Return_Rate))+
  theme_bw(base_family = "STKaiti")+
  scale_color_gradient(name="退回\n百分比", low="blue", high="red")+
  scale_size_continuous(name = "销量",range = c(1,6)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  ggtitle("销量前100产品&线路")+ xlab("销售线路ID")+ylab("产品ID")


## 产品和销售路线 top50
top50routes <- routes$Ruta_SAK[1:50]
top50products <- products$Producto_ID[1:50]

ggplot(routes.products %>% 
         filter(Ruta_SAK %in% top50routes, Producto_ID %in% top50products))+
  geom_point(aes(x=as.character(Ruta_SAK), 
                 y=as.character(Producto_ID), 
                 size=Units, color=Return_Rate))+
  theme_bw(base_family = "STKaiti")+
  scale_color_gradient(name="退回\n百分比", low="blue", high="red")+
  scale_size_continuous(name = "销量",range = c(1,8)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  ggtitle("销量前50产品&线路")+ xlab("销售线路ID")+ylab("产品ID")

##  产品和客户
products_by_client <- traindata %>%
  group_by(Cliente_ID) %>%
  summarise(n_products = n_distinct(Producto_ID)) %>%
  inner_join(cliente_tabla, by="Cliente_ID")

ggplot(products_by_client)+
  geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=2)+
  theme_bw(base_family = "STKaiti")+
  scale_y_continuous(labels=function(x)paste(x/1000, "k"))+
  ggtitle("产品量所对应的客户量")+ xlab("产品数量(种)")+ylab("客户数量(位)")



