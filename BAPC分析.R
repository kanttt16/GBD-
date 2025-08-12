#BAPC
install.packages("remotes")
install.packages("foreach")


install.packages("cmprsk")
install.packages("fanplot")
install.packages("Epi")
install.packages("caTools")
install.packages("BAPC",repos = "http://R-Forge.R-project.org")
install.packages("sp")
install.packages("INLA")
install.packages("epitools")
install.packages("nordpred", repos = "http://R-Forge.R-project.org")
install.packages("fmesher")

library(BAPC)

library(openxlsx)

library(INLA)

source("D:/R Windows/R-4.5.0/library/nordpred.s")

library(reshape)
library(tidyverse)
library(Epi)
library(epitools)
library(ggplot2)
library(readxl)

#初始化

#发病
age1 <- c(
  "<5 years",
  "5-9 years",
  "10-14 years",
  "15-19 years",
  "20-24 years",
  "25-29 years",
  "30-34 years",
  "35-39 years",
  "40-44 years",
  "45-49 years",
  "50-54 years",
  "55-59 years",
  "60-64 years",
  "65-69 years",
  "70-74 years",
  "75-79 years",
  "80-84 years",
  "85-89 years",
  "90-94 years",
  "95+ years"
)


#标准
age2 <- c(
  "<1 years",
  "1 to 4",
  "5 to 9",
  "10 to 14",
  "15 to 19",
  "20 to 24",
  "25 to 29",
  "30 to 34",
  "35 to 39",
  "40 to 44",
  "45 to 49",
  "50 to 54",
  "55 to 59",
  "60 to 64",
  "65 to 69",
  "70 to 74",
  "75 to 79",
  "80 to 84",
  "85 to 89",
  "90 to 94",
  "95 plus"
)


#预测
age3 <- c("0 to 4",
          "5 to 9",
          "10 to 14",
          "15 to 19",
          "20 to 24",
          "25 to 29",
          "30 to 34",
          "35 to 39",
          "40 to 44",
          "45 to 49",
          "50 to 54",
          "55 to 59",
          "60 to 64",
          "65 to 69",
          "70 to 74",
          "75 to 79",
          "80 to 84",
          "85 to 89",
          "90 to 94",
          "95 plus")




age_stand <- read_xlsx("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\BAPC\\age_stand.xlsx")

sum(age_stand$std_population)

#标准构成比
wstand <- c(age_stand$std_population[1:2]%>%as.numeric()%>%sum(),
            age_stand$std_population[3:21]%>%as.numeric())/sum(age_stand$std_population[1:21])


gallbladder_and_biliary1 <- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary1.csv")
gallbladder_and_biliary2 <- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary2.csv")

gallbladder <- rbind(gallbladder_and_biliary1,gallbladder_and_biliary2)

gallbladder <- gallbladder %>%
  mutate(measure = if_else(str_detect(measure, "DALYs"), "DALYs", measure))




#incidence

fig9_A_1 <- filter(gallbladder,
                   gallbladder$age %in% age1,
                   gallbladder$sex=="Both",
                   gallbladder$location=="Global",
                   gallbladder$metric=="Number",
                   gallbladder$measure=="Incidence")

unique(fig9_A_1$age)

fig9_A_1$age <- gsub(" years","",fig9_A_1$age)

fig9_A_1$age <- factor(fig9_A_1$age,levels = c(
  "<5",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80-84",
  "85-89",
  "90-94",
  "95+"
))

fig9_A_2 <- fig9_A_1[,c(1,4,7,8)]

#转表
fig9_A_2 <- reshape2::dcast(data = fig9_A_2,year~age,value.var = "val")

rownames(fig9_A_2) <- fig9_A_2$year
fig9_A_2 <- fig9_A_2[,-1]

fig9_A_3 <- apply(fig9_A_2,c(1,2),round)%>%as.data.frame()


#人口数据
var_name <- c("location_name","sex_name","year","age_name","val")

pop <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\BAPC\\pop_global.csv")


age2 <- c(
  "<1 years",
  "2-4 years",
  "<5 years",
  "5-9 years",
  "10-14 years",
  "15-19 years",
  "20-24 years",
  "25-29 years",
  "30-34 years",
  "35-39 years",
  "40-44 years",
  "45-49 years",
  "50-54 years",
  "55-59 years",
  "60-64 years",
  "65-69 years",
  "70-74 years",
  "75-79 years",
  "80-84 years",
  "85-89 years",
  "90-94 years",
  "95+ years"
)

fig9_A_pop <-  pop%>%dplyr::select(var_name)

fig9_A_pop <- fig9_A_pop%>%  filter(age_name %in% age2)


fig9_A_pop$age_name <- gsub(" years","",fig9_A_pop$age_name)

fig9_A_pop <- fig9_A_pop%>%filter(sex_name=="Both")

unique(fig9_A_pop$age_name)

#预测人口学数据
pre_var_name <- c("location_name","sex","year_id","age_group_name","val")

fig9_A_pre_pop <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\BAPC\\IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv")

fig9_A_pre_pop_1 <- fig9_A_pre_pop%>%
  dplyr:: select(pre_var_name)%>%
  filter(location_name=="Global",
         year_id %in% c(2022:2036))

fig9_A_pre_pop_male <- fig9_A_pre_pop_1%>%filter(sex=="Male")
fig9_A_pre_pop_female <- fig9_A_pre_pop_1%>%filter(sex=="Female")

fig9_A_pre_pop_both <- cbind(fig9_A_pre_pop_female,fig9_A_pre_pop_male)

colnames(fig9_A_pre_pop_both)[c(5,10)] <- c("val1","val2")
fig9_A_pre_pop_both <- fig9_A_pre_pop_both[,-c(2,6,8,9)]
fig9_A_pre_pop_both <- fig9_A_pre_pop_both%>%mutate(val_both=val1+val2)
fig9_A_pre_pop_both <- fig9_A_pre_pop_both%>%mutate(sex="Both")
fig9_A_pre_pop_both <- fig9_A_pre_pop_both[,-c(4,6)]

fig9_A_pre_pop_5year <- fig9_A_pre_pop_both%>%
  filter(age_group_name %in%c("Early Neonatal","Late Neonatal","Post Neonatal","1 to 4"))%>%
  group_by(year_id)%>%
  summarise(val=sum(val_both))%>%
  mutate(age_group_name="<5")


fig9_A_pre_pop_both <- fig9_A_pre_pop_both%>%filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal","Post Neonatal","1 to 4")))


colnames(fig9_A_pre_pop_both)[5] <- "val"

fig9_A_pre_pop_5year <- fig9_A_pre_pop_5year%>%mutate(location_name="Global",sex="Both")
fig9_A_pre_pop_both <- rbind(fig9_A_pre_pop_5year,fig9_A_pre_pop_both)

names(fig9_A_pre_pop_both)[names(fig9_A_pre_pop_both)=='age_group_name'] <- 'age_name'

fig9_A_pre_pop_both$age_name <- gsub(" to ","-",fig9_A_pre_pop_both$age_name)
fig9_A_pre_pop_both$age_name <- gsub(" plus","+",fig9_A_pre_pop_both$age_name)

colnames(fig9_A_pre_pop_both)[5] <- "sex_name"
colnames(fig9_A_pre_pop_both)[1] <-"year"
unique(fig9_A_pre_pop_both$age_name)


fig9_A_pop_bind <- rbind(fig9_A_pop,fig9_A_pre_pop_both)

unique(fig9_A_pop_bind$age_name)

fig9_A_pop_bind <- fig9_A_pop_bind%>%filter(!age_name %in% c("2-4","All Ages"))

fig9_A_pop_bind$age_name <- factor(fig9_A_pop_bind$age_name,levels =c(
  "<5",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80-84",
  "85-89",
  "90-94",
  "95+"
))

fig9_A_pop_bind_2 <-reshape2::dcast(data=fig9_A_pop_bind,year~age_name,value.var = ("val"))%>%as.data.frame()
fig9_A_pop_bind_2 <- fig9_A_pop_bind_2%>%filter(!(year >= 1950 & year <= 1989))


rownames(fig9_A_pop_bind_2) <- fig9_A_pop_bind_2$year
fig9_A_pop_bind_2 <- fig9_A_pop_bind_2[,-1]

fig9_A_pop_bind_2 <- apply(fig9_A_pop_bind_2,c(1,2),as.numeric)%>%as.data.frame()
fig9_A_pop_bind_2 <- apply(fig9_A_pop_bind_2,c(1,2),round)%>%as.data.frame()





#患病数据

fig9_A_pop_pro <- matrix(data=NA,nrow = 2036-2021,ncol = ncol(fig9_A_pop_bind_2))%>%as.data.frame()
rownames(fig9_A_pop_pro) <- seq(2022,2036,1)
colnames(fig9_A_pop_pro) <- names(fig9_A_pop_bind_2)

fig9_A_pop_pro_2 <- rbind(fig9_A_3,fig9_A_pop_pro)

fig9_A_pop_pro_2 <- apply(fig9_A_pop_pro_2,c(1,2),as.numeric)%>%as.data.frame()
fig9_A_pop_pro_2 <- apply(fig9_A_pop_pro_2,c(1,2),round)%>%as.data.frame()



require(INLA)

diabetes_input <- APCList(fig9_A_pop_pro_2,fig9_A_pop_bind_2,gf=5)

diabetes_input_re <- BAPC(diabetes_input,predict = list(npredict=15,retro=T),secondDiff = FALSE,stdweight = wstand,verbose = F)

p1 <- plotBAPC(diabetes_input_re,scale = 10^5,type = 'ageStdRate',showdata = T)

































































