
#搭建SII分析的工作流

#读取
library(tidyverse)
library(data.table)
library(car)
library(MASS)
library(mgcv)
library(splines)
library(broom)
library(ggplot2)
library(readxl)
library(writexl)







#-------------------------------------------------------------------------------


#人口数据
pop <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\fig6&fig8\\fig6_8_allage_population.csv")
#源数据
fig8_data <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\fig6&fig8\\gallbladder and biliary_all_age.csv")
#sdi数据
SDI <- read_xlsx("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\SDI数据.xlsx")





#源数据fig8_A_data要分为四层，分别是DALYS ，incidence,prevalence,deaths,下面以dalys为例
#DALYS
fig8_E_data <- filter(fig8_data,
                      measure=="DALYs (Disability-Adjusted Life Years)",
                      sex=="Both",
                      year %in% c("1990","2021")
)


#SDI转
library(janitor)
SDI <- janitor::row_to_names(SDI, row_number = 1)

SDI <- SDI%>%
  pivot_longer(cols = `1990`:`2021`,names_to = "year")%>%
  rename(sdi=value)

colnames(SDI)[1] <- "location"
SDI$year <- as.integer(SDI$year)

#因为三个表的location各不相同，所以以源数据为主做个分析
fig8_E_data_1 <- left_join(fig8_E_data,SDI,by=c("location","year"))

fig8_E_data_2 <- left_join(fig8_E_data_1,pop,by=c("location","sex","year"))

#至此先处理na值

fig8_E_data_3 <- fig8_E_data_2%>%drop_na()

#各个列的重命名和删除
fig8_E_data_3 <- fig8_E_data_3[,-c(5,9,10,12,13,16,17)]
colnames(fig8_E_data_3)[7] <-"val"
colnames(fig8_E_data_3)[10] <-"pop"


#计算人口权重
a <- fig8_E_data_3%>%
  filter(metric.x=="Number")%>%
  group_by(year)%>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]


rank <- fig8_E_data_3 %>%
  mutate(pop_global = ifelse(year==1990,pop1990,pop2021))%>%
  group_by(year,metric.x)%>%
  arrange(sdi)%>%
  mutate(cummu=cumsum(pop))%>%
  mutate(half=pop/2)%>%
  mutate(midpoint=cummu-half)%>%
  mutate(weight_order=midpoint/pop_global)

rank$year <- factor(rank$year)


#计算回归
temp1 <- rank%>%
  filter(metric.x=="Rate",year==1990)

temp2 <- rank%>%
  filter(metric.x=="Rate",year==2021)

fit1 <- lm(data=temp1,val~weight_order)
fit2 <- lm(data=temp2,val~weight_order)

coef(fit1)#截距-17.2625，斜率172.9862
coef(fit2)#截距16.6746，斜率149.6248

ncvTest(fit1)
ncvTest(fit2)

#如果简单线性回归不太准，采用rlm
r.huber1 <- rlm(data=temp1,val~weight_order)
r.huber2 <- rlm(data=temp2,val~weight_order)

confint.default(r.huber1)
confint.default(r.huber2)
#

#绘图
b1 <- coef(r.huber1)
b2 <- coef(r.huber2)


color <- c("#6699FF","#990000")

p1 <- rank%>%
  filter(metric.x=="Rate")%>%
  ggplot(aes(x=weight_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_segment(aes(x = 0.02, xend = 0.99,
                   y = b1[1] + b1[2]*0.02,
                   yend = b1[1] + b1[2]*0.99),
               color = color[1], size = 0.8) +
  geom_segment(aes(x = 0.02, xend = 0.99,
                   y = b2[1] + b2[2]*0.02,
                   yend = b2[1] + b2[2]*0.99),
               color = color[2], size = 0.8)
p1


#看一看差值
range_1990 <- rank %>%
  filter(metric.x == "Rate", year == 1990) %>%
  summarise(delta = max(val) - min(val)) %>%
  pull(delta)

range_2021 <- rank %>%
  filter(metric.x == "Rate", year == 2021) %>%
  summarise(delta = max(val) - min(val)) %>%
  pull(delta)








#集中指数

b <- fig8_E_data_3 %>%
     filter(metric.x=="Number")%>%
     group_by(year)%>%
     summarise(sum=sum(val))

daly1990 <- b$sum[1]
daly2021 <- b$sum[2]
 
ci <- rank%>%
  filter(metric.x=="Number")%>%
  mutate(total_daly=ifelse(year==1990,daly1990,daly2021))%>%
  group_by(year)%>%
  arrange(sdi)%>%
  mutate(cummu_daly=cumsum(val))%>%
  mutate(frac_daly=cummu_daly/total_daly)%>%
  mutate(frac_population=cummu/pop_global)
temp3 <- ci%>%
  filter(metric.x=="Number",year==1990)
 
temp4 <- ci%>%
  filter(metric.x=="Number",year==2021)
##计算集中指数
CI_1990 <- 2*(sum(temp3$frac_daly)/nrow(temp3))-1
CI_2021 <- 2*(sum(temp4$frac_daly)/nrow(temp4))-1



#绘图
p2 <- ci%>%
  ggplot(aes(x=frac_population,y=frac_daly,fill = year,
             color=year,group = year))+
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype = 1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype = 1,size=1,color="gray")+
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               linetype = 1,size=0.7,color="#CD835F",alpha=0.1)+
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,sahpe=21)+
  scale_fill_manual(values = color)+
  geom_smooth(method = "gam",
              formula = y~ns(x,
                             knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),
                             Boundary.knots = c(0,1)),
              linetype=1,size=0.1)+
  scale_color_manual(values = color)+
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label=paste0("CI 1990 = ", round(CI_1990, 3)),x=0.75,y=0.3,size=3)+
  annotate("text",label=paste0("CI 2021 = ", round(CI_1990, 3)),x=0.75,y=0.25,size=3)+
  theme_bw()


p2




#-------------------------------------------------------------------------------

#人口数据
pop <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\fig6&fig8\\fig6_8_allage_population.csv")
#源数据
fig8_data <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\fig6&fig8\\gallbladder and biliary_all_age.csv")
#sdi数据
SDI <- read_xlsx("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\SDI数据.xlsx")


#Incidence
fig8_A_data <- filter(fig8_data,
                      measure=="Incidence",
                      sex=="Both",
                      year %in% c("1990","2021")
)


#SDI转
library(janitor)
SDI <- janitor::row_to_names(SDI, row_number = 1)

SDI <- SDI%>%
  pivot_longer(cols = `1990`:`2021`,names_to = "year")%>%
  rename(sdi=value)

colnames(SDI)[1] <- "location"
SDI$year <- as.integer(SDI$year)

#因为三个表的location各不相同，所以以源数据为主做个分析
fig8_A_data_1 <- left_join(fig8_A_data,SDI,by=c("location","year"))

fig8_A_data_2 <- left_join(fig8_A_data_1,pop,by=c("location","sex","year"))

#至此先处理na值

fig8_A_data_3 <- fig8_A_data_2%>%drop_na()

#各个列的重命名和删除
fig8_A_data_3 <- fig8_A_data_3[,-c(5,9,10,12,13,16,17)]
colnames(fig8_A_data_3)[7] <-"val"
colnames(fig8_A_data_3)[10] <-"pop"


#计算人口权重
a <- fig8_A_data_3%>%
  filter(metric.x=="Number")%>%
  group_by(year)%>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]


rank <- fig8_A_data_3 %>%
  mutate(pop_global = ifelse(year==1990,pop1990,pop2021))%>%
  group_by(year,metric.x)%>%
  arrange(sdi)%>%
  mutate(cummu=cumsum(pop))%>%
  mutate(half=pop/2)%>%
  mutate(midpoint=cummu-half)%>%
  mutate(weight_order=midpoint/pop_global)

rank$year <- factor(rank$year)


#计算回归
temp1 <- rank%>%
  filter(metric.x=="Rate",year==1990)

temp2 <- rank%>%
  filter(metric.x=="Rate",year==2021)

fit1 <- lm(data=temp1,val~weight_order)
fit2 <- lm(data=temp2,val~weight_order)

coef(fit1)#截距-17.2625，斜率172.9862
coef(fit2)#截距16.6746，斜率149.6248

ncvTest(fit1)
ncvTest(fit2)

#如果简单线性回归不太准，采用rlm
r.huber1 <- rlm(data=temp1,val~weight_order)
r.huber2 <- rlm(data=temp2,val~weight_order)

confint.default(r.huber1)
confint.default(r.huber2)
#

#绘图
b1 <- coef(r.huber1)
b2 <- coef(r.huber2)


color <- c("#fff000","#00ccff")

p1 <- rank%>%
  filter(metric.x=="Rate")%>%
  ggplot(aes(x=weight_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_segment(aes(x = 0.02, xend = 0.99,
                   y = b1[1] + b1[2]*0.02,
                   yend = b1[1] + b1[2]*0.99),
               color = color[1], size = 0.8) +
  geom_segment(aes(x = 0.02, xend = 0.99,
                   y = b2[1] + b2[2]*0.02,
                   yend = b2[1] + b2[2]*0.99),
               color = color[2], size = 0.8)
p1


#看一看差值
range_1990 <- rank %>%
  filter(metric.x == "Rate", year == 1990) %>%
  summarise(delta = max(val) - min(val)) %>%
  pull(delta)

range_2021 <- rank %>%
  filter(metric.x == "Rate", year == 2021) %>%
  summarise(delta = max(val) - min(val)) %>%
  pull(delta)








#集中指数

b <- fig8_A_data_3 %>%
  filter(metric.x=="Number")%>%
  group_by(year)%>%
  summarise(sum=sum(val))

daly1990 <- b$sum[1]
daly2021 <- b$sum[2]

ci <- rank%>%
  filter(metric.x=="Number")%>%
  mutate(total_daly=ifelse(year==1990,daly1990,daly2021))%>%
  group_by(year)%>%
  arrange(sdi)%>%
  mutate(cummu_daly=cumsum(val))%>%
  mutate(frac_daly=cummu_daly/total_daly)%>%
  mutate(frac_population=cummu/pop_global)
temp3 <- ci%>%
  filter(metric.x=="Number",year==1990)

temp4 <- ci%>%
  filter(metric.x=="Number",year==2021)
##计算集中指数
CI_1990 <- 2*(sum(temp3$frac_daly)/nrow(temp3))-1
CI_2021 <- 2*(sum(temp4$frac_daly)/nrow(temp4))-1



#绘图
p2 <- ci%>%
  ggplot(aes(x=frac_population,y=frac_daly,fill = year,
             color=year,group = year))+
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype = 1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype = 1,size=1,color="gray")+
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               linetype = 1,size=0.7,color="#CD835F",alpha=0.1)+
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,sahpe=21)+
  scale_fill_manual(values = color)+
  geom_smooth(method = "gam",
              formula = y~ns(x,
                             knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),
                             Boundary.knots = c(0,1)),
              linetype=1,size=0.1)+
  scale_color_manual(values = color)+
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label=paste0("CI 1990 = ", round(CI_1990, 3)),x=0.75,y=0.3,size=3)+
  annotate("text",label=paste0("CI 2021 = ", round(CI_1990, 3)),x=0.75,y=0.25,size=3)+
  theme_bw()


p2












































