library(tidyverse)
library(ggplot2)
library(writexl)
library(readxl)
library(ggsci)
library(patchwork)
rm(list = ls())
gallbladder_and_biliary1 <- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary1.csv")
gallbladder_and_biliary2 <- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary2.csv")

gallbladder <- rbind(gallbladder_and_biliary1,gallbladder_and_biliary2)

gallbladder <- gallbladder %>%
  mutate(measure = if_else(str_detect(measure, "DALYs"), "DALYs", measure))


#先做fig1下的四张图
#A
#数据准备
fig1_a_gallbladder <- gallbladder %>% filter(measure == "Incidence",
                                            metric == "Rate",
                                            sex =="Both",
                                            age =="Age-standardized"
                                            )
#unique函数提取唯一变量
locations <- unique(fig1_a_gallbladder$location)

#用lapply函数，它的工作流是是把locations的每个地区，都执行一遍function(loc)，也就是
#说loc是locations数据框的单元素子集，对其进行循环
#这意味着lapply执行对于字符型数字型等数据框内“无关联”数据的历遍循环
plots <- lapply(locations, function(loc) {
  df <- fig1_a_gallbladder %>% filter(location == loc)
  
  val_1990 <- df %>% filter(year == 1990) %>% pull(val)#传参
  val_2021 <- df %>% filter(year == 2021) %>% pull(val)#
  
  if(length(val_1990) == 0 || length(val_2021) == 0) return(NULL)
  
  #绘图
  ggplot(df, aes(x = year, y = val)) +
    geom_line(color = "steelblue", size = 1) +
    #标注1990年和2021年的文本并调整位置
    geom_text(data = data.frame(year = 1990, val = val_1990),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = 1.1, vjust = 0.5, size = 3) +
    geom_text(data = data.frame(year = 2021, val = val_2021),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = -0.1, vjust = 0.5, size = 3) +
    labs(title = loc) +
    #cood_cartesian的clip = "off"识别不完全显示的部分并且拓展画幅
    coord_cartesian(ylim = range(df$val, na.rm = TRUE), clip = "off") +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.margin = margin(t = 5, r = 20, b = 5, l = 20)
    )
})

plots <- Filter(Negate(is.null), plots)
#合并循环好的所有图幅
all_plots <- wrap_plots(plots, ncol = 1)
#以pdf方式保存并调整高度大小
ggsave("gallbladder_fig1_a.pdf", all_plots, width = 6, height = 0.9 * length(plots), limitsize = FALSE)

#---------------------------------------------------------------------------------
#B
#数据准备
fig1_b_gallbladder <- gallbladder %>% filter(measure == "Prevalence",
                                            metric == "Rate",
                                            sex =="Both",
                                            age =="Age-standardized"
)
#unique函数提取唯一变量
locations <- unique(fig1_b_gallbladder$location)

#用lapply函数，它的工作流是是把locations的每个地区，都执行一遍function(loc)，也就是
#说loc是locations数据框的单元素子集，对其进行循环
#这意味着lapply执行对于字符型数字型等数据框内“无关联”数据的历遍循环
plots <- lapply(locations, function(loc) {
  df <- fig1_b_gallbladder %>% filter(location == loc)
  
  val_1990 <- df %>% filter(year == 1990) %>% pull(val)#传参
  val_2021 <- df %>% filter(year == 2021) %>% pull(val)#
  
  if(length(val_1990) == 0 || length(val_2021) == 0) return(NULL)
  
  #绘图
  ggplot(df, aes(x = year, y = val)) +
    geom_line(color = "steelblue", size = 1) +
    #标注1990年和2021年的文本并调整位置
    geom_text(data = data.frame(year = 1990, val = val_1990),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = 1.1, vjust = 0.5, size = 3) +
    geom_text(data = data.frame(year = 2021, val = val_2021),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = -0.1, vjust = 0.5, size = 3) +
    labs(title = loc) +
    #cood_cartesian的clip = "off"识别不完全显示的部分并且拓展画幅
    coord_cartesian(ylim = range(df$val, na.rm = TRUE), clip = "off") +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.margin = margin(t = 5, r = 20, b = 5, l = 20)
    )
})

plots <- Filter(Negate(is.null), plots)
#合并循环好的所有图幅
all_plots <- wrap_plots(plots, ncol = 1)
#以pdf方式保存并调整高度大小
ggsave("gallbladder_fig1_b.pdf", all_plots, width = 6, height = 0.9 * length(plots), limitsize = FALSE)



#---------------------------------------------------------------------------------
#C
#数据准备
fig1_c_gallbladder <- gallbladder %>% filter(measure == "Deaths",
                                            metric == "Rate",
                                            sex =="Both",
                                            age =="Age-standardized"
)
#unique函数提取唯一变量
locations <- unique(fig1_c_gallbladder$location)

#用lapply函数，它的工作流是是把locations的每个地区，都执行一遍function(loc)，也就是
#说loc是locations数据框的单元素子集，对其进行循环
#这意味着lapply执行对于字符型数字型等数据框内“无关联”数据的历遍循环
plots <- lapply(locations, function(loc) {
  df <- fig1_c_gallbladder %>% filter(location == loc)
  
  val_1990 <- df %>% filter(year == 1990) %>% pull(val)#传参
  val_2021 <- df %>% filter(year == 2021) %>% pull(val)#
  
  if(length(val_1990) == 0 || length(val_2021) == 0) return(NULL)
  
  #绘图
  ggplot(df, aes(x = year, y = val)) +
    geom_line(color = "steelblue", size = 1) +
    #标注1990年和2021年的文本并调整位置
    geom_text(data = data.frame(year = 1990, val = val_1990),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = 7, vjust = 0.5, size = 3) +
    geom_text(data = data.frame(year = 2021, val = val_2021),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = -0.1, vjust = 0.5, size = 3) +
    labs(title = loc) +
    #cood_cartesian的clip = "off"识别不完全显示的部分并且拓展画幅
    coord_cartesian(ylim = range(df$val, na.rm = TRUE), clip = "off") +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.margin = margin(t = 5, r = 20, b = 5, l = 20)
    )
})

plots <- Filter(Negate(is.null), plots)
#合并循环好的所有图幅
all_plots <- wrap_plots(plots, ncol = 1)
#以pdf方式保存并调整高度大小
ggsave("gallbladder_fig1_c.pdf", all_plots, width = 6, height = 0.9 * length(plots), limitsize = FALSE)


#---------------------------------------------------------------------------------
#D
#数据准备
fig1_d_gallbladder <- gallbladder %>% filter(measure == "DALYs (Disability-Adjusted Life Years)",
                                             metric == "Rate",
                                             sex =="Both",
                                             age =="Age-standardized"
)
#unique函数提取唯一变量
locations <- unique(fig1_d_gallbladder$location)

#用lapply函数，它的工作流是是把locations的每个地区，都执行一遍function(loc)，也就是
#说loc是locations数据框的单元素子集，对其进行循环
#这意味着lapply执行对于字符型数字型等数据框内“无关联”数据的历遍循环
plots <- lapply(locations, function(loc) {
  df <- fig1_d_gallbladder %>% filter(location == loc)
  
  val_1990 <- df %>% filter(year == 1990) %>% pull(val)#传参
  val_2021 <- df %>% filter(year == 2021) %>% pull(val)#
  
  if(length(val_1990) == 0 || length(val_2021) == 0) return(NULL)
  
  #绘图
  ggplot(df, aes(x = year, y = val)) +
    geom_line(color = "steelblue", size = 1) +
    #标注1990年和2021年的文本并调整位置
    geom_text(data = data.frame(year = 1990, val = val_1990),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = 1.1, vjust = 0.5, size = 3) +
    geom_text(data = data.frame(year = 2021, val = val_2021),
              aes(x = year, y = val, label = round(val, 1)),
              hjust = -0.1, vjust = 0.5, size = 3) +
    labs(title = loc) +
    #cood_cartesian的clip = "off"识别不完全显示的部分并且拓展画幅
    coord_cartesian(ylim = range(df$val, na.rm = TRUE), clip = "off") +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.margin = margin(t = 5, r = 20, b = 5, l = 20)
    )
})

plots <- Filter(Negate(is.null), plots)
#合并循环好的所有图幅
all_plots <- wrap_plots(plots, ncol = 1)
#以pdf方式保存并调整高度大小
ggsave("gallbladder_fig1_d.pdf", all_plots, width = 6, height = 0.9 * length(plots), limitsize = FALSE)


#-----------------------------------------------------------------------------------------------------------------

#Fig2
#A
#求incidence percentage change in cases(但是没说哪个年龄段)
fig2_a_gallbladder <- gallbladder %>% filter(measure == "Incidence",
                                             metric == "Number",
                                             year %in% c(1990,2021))
fig2_a_gallbladder <- fig2_a_gallbladder[,c(2,3,4,7,8)]


fig2_a_gallbladder_1990 <- fig2_a_gallbladder%>%filter(year == "1990")
fig2_a_gallbladder_2021 <- fig2_a_gallbladder%>%filter(year == "2021")


fig2_a_gallbladder_1990$per_change <- round(
  (fig2_a_gallbladder_2021$val - fig2_a_gallbladder_1990$val) / fig2_a_gallbladder_1990$val,2
)

fig2_a_gallbladder_1990 <- fig2_a_gallbladder_1990[,-c(4,5)]

fig2_a_gallbladder_1990 <- fig2_a_gallbladder_1990 %>%
  group_by(location, sex, age) %>%
  summarise(per_change = min(per_change, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = age,
    values_from = per_change,
    names_prefix = "age_"
  )



fig2_a_gallbladder_1990 <- fig2_a_gallbladder_1990 %>%
  mutate(sum = rowSums(across(starts_with("age"), ~ as.numeric(.)), na.rm = TRUE))
#----------------------------------------------------------------------------------------
#傻逼作者我草你吗呀，你他妈不会在table上写一个用的是all ages的数据啊

all_age<- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary_all_age.csv")
all_age <- all_age %>%filter(year%in%c("1990","2021"),metric == "Number",measure=="Incidence")
all_age <- all_age[,c(2,3,7,8)]

all_age <- all_age %>%
  group_by(location, sex,year) %>%
  pivot_wider(
    names_from = year,
    values_from = val,
    values_fn = mean 
  )


all_age$per_change <- round((all_age$`2021`-all_age$`1990`)/all_age$`1990`*100,3)

ggplot(all_age,mapping = aes(x = per_change,y = location,colour = sex,shape = sex))+
  geom_point(size = 5)+
  scale_shape_manual(values = c("Both" = 16, "Male" = 17, "Female" = 15))+
  scale_x_continuous(breaks = seq(-10,150,by=50))+
  scale_color_manual(values = c(
    "Both" = "#003366",  # 深蓝
    "Male" = "#66b2ff",  # 浅蓝
    "Female" = "#ff3300" # 鲜红
  )) + 
  theme_minimal()

#-------------------------------------------------------------------------------
#B
fig2_b_gallbladder <- gallbladder %>% filter(measure == "Incidence",
                                             metric == "Rate",
                                             year %in% c(1990,2021),
                                             age == "Age-standardized")

fig2_b_gallbladder <- fig2_b_gallbladder[,c(2,3,7,8)]
fig2_b_gallbladder <- fig2_b_gallbladder%>%group_by(location, sex,year) %>%
  pivot_wider(
    names_from = year,
    values_from = val,
    values_fn = mean 
  )

fig2_b_gallbladder$per_change <- round((fig2_b_gallbladder$`2021`-fig2_b_gallbladder$`1990`)/fig2_b_gallbladder$`1990`*100,3)

ggplot(fig2_b_gallbladder,mapping = aes(x = per_change,y = location,colour = sex,shape = sex))+
  geom_point(size = 5)+
  scale_shape_manual(values = c("Both" = 16, "Male" = 17, "Female" = 15))+
  scale_x_continuous(breaks = seq(-20,10,by=10))+
  scale_color_manual(values = c(
    "Both" = "#003366",  # 深蓝
    "Male" = "#66b2ff",  # 浅蓝
    "Female" = "#ff3300" # 鲜红
  )) + 
  theme_minimal()

#-------------------------------------------------------------------------------
#C
#age-standardized下的eapc
fig2_c_gallbladder <- gallbladder %>% filter(measure == "Incidence",
                                             metric == "Rate",
                                             age == "Age-standardized")
view(fig2_c_gallbladder)

calculate_EAPC_forloop <- function(df) {
  if (!all(c("location", "year", "val", "sex", "age") %in% colnames(df))) {
    stop("数据必须包含 location、year、val、sex 和 age 列")
  }
  
  # 获取所有唯一的 location-age-sex 组合
  groups <- unique(df[, c("location", "age", "sex")])
  
  # 创建空结果数据框
  result <- data.frame(
    location = character(),
    age = character(),
    sex = character(),
    EAPC = numeric(),
    LCI = numeric(),
    UCI = numeric(),
    CI = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(groups)) {
    loc <- groups$location[i]
    ag <- groups$age[i]
    sx <- groups$sex[i]
    
    sub_df <- subset(df, location == loc & age == ag & sex == sx)
    
    if (nrow(sub_df) >= 2 && all(sub_df$val > 0)) {
      sub_df$y <- log(sub_df$val)
      model <- lm(y ~ year, data = sub_df)
      beta <- summary(model)[["coefficients"]][2, 1]
      se <- summary(model)[["coefficients"]][2, 2]
      
      EAPC <- 100 * (exp(beta) - 1)
      LCI <- 100 * (exp(beta - 1.96 * se) - 1)
      UCI <- 100 * (exp(beta + 1.96 * se) - 1)
      CI <- paste0(round(EAPC, 2), " (", round(LCI, 2), "-", round(UCI, 2), ")")
      
      result <- rbind(result, data.frame(
        location = loc,
        age = ag,
        sex = sx,
        EAPC = round(EAPC, 2),
        LCI = round(LCI, 2),
        UCI = round(UCI, 2),
        CI = CI,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(result)
}


fig2_c_gallbladder_EAPC <- calculate_EAPC_forloop(fig2_c_gallbladder)

ggplot(fig2_c_gallbladder_EAPC  ,mapping = aes(x = EAPC,y = location,colour = sex,shape = sex))+
  geom_point(size = 5)+
  scale_shape_manual(values = c("Both" = 16, "Male" = 17, "Female" = 15))+
  scale_x_continuous(breaks = seq(-0.4,0.4,by=0.4))+
  scale_color_manual(values = c(
    "Both" = "#003366",  # 深蓝
    "Male" = "#66b2ff",  # 浅蓝
    "Female" = "#ff3300" # 鲜红
  )) + 
  theme_minimal()

#-------------------------------------------------------------------------------
#table1的思路
#最好是按照性别来排布，三个性别一个个做，下面以both为例
#第一个Both_ Incidence_ Percentage change in ASRs from 1990 to 2021
#是measure == "Incidence",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized"

#第二三四个Both_ Incidence_ Percentage change in Number from 1990 to 2021，Both_Incidence_No, in cases (95% UI)_2021，Both_ Incidence_ASR per 100 000 (95% UI)_1990 
#是year%in%c("1990","2021"),metric == "Number",measure=="Incidence",age=="all age"

#第五六个Both_ Incidence_ASR per 100 000 (95% UI)_1990 ，Both_ Incidence_ASR per 100 000 (95% UI)_2021 
#是sex=="Both",measure=="Incidence",age=="Age-standardized",year %in% c(1990,2021),metric=="rate"

#第七EAPC

Table1_1 <- gallbladder%>%filter(measure == "Incidence",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")

Table1_1 <- Table1_1[,c(2,3,7,8)]


Table1_1 <- Table1_1%>%group_by(location, sex,year) %>%
  pivot_wider(
    names_from = year,
    values_from = val,
    values_fn = mean 
  )
Table1_1$per_change <- round((Table1_1$`2021`-Table1_1$`1990`)/Table1_1$`1990`*100,3)
Table1_1 <- Table1_1[,-c(3,4)]
Table1_1 <- Table1_1%>%pivot_wider(names_from = sex,
                                   values_from = per_change)
#至此写好了Both/Male/Female Incidence_ Percentage change in ASRs from 1990 to 2021
#-------------------------------------------------------------------------------
#234列及其对应的三种性别
Table1_234<- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary_all_age.csv")
Table1_234 <- Table1_234 %>%filter(year%in%c("1990","2021"),metric == "Number",measure=="Incidence")
Table1_234 <- Table1_234[,c(2,3,7,8)]

Table1_234 <- Table1_234%>%pivot_wider(names_from = year,
                                       values_from = val,
                                       values_fn = mean)

Table1_234$per_change <- round((Table1_234$`2021`-Table1_234$`1990`)/Table1_234$`1990`*100,3)
Table1_234 <- Table1_234[,-c(3,4)]
Table1_234 <- Table1_234%>%pivot_wider(names_from = sex,
                                       values_from = per_change)

#-------------------------------------------------------------------------------
#5,6列
Table1_56 <- gallbladder%>%filter(measure == "Incidence",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")
Table1_56 <- Table1_56[,c(2,3,7,8)]
Table1_56 <- Table1_56%>%pivot_wider(
  names_from = c(sex, year),
  values_from = val,
  names_sep = "_",      
  values_fn = mean     
)

#-------------------------------------------------------------------------------
#第七列
#eapc计算函数，分三层，匹配唯一的(global,sex,age)
calculate_EAPC_forloop <- function(df) {
  if (!all(c("location", "year", "val", "sex", "age") %in% colnames(df))) {
    stop("数据必须包含 location、year、val、sex 和 age 列")
  }
  
  # 获取所有唯一的 location-age-sex 组合
  groups <- unique(df[, c("location", "age", "sex")])
  
  # 创建空结果数据框
  result <- data.frame(
    location = character(),
    age = character(),
    sex = character(),
    EAPC = numeric(),
    LCI = numeric(),
    UCI = numeric(),
    CI = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(groups)) {
    loc <- groups$location[i]
    ag <- groups$age[i]
    sx <- groups$sex[i]
    
    sub_df <- subset(df, location == loc & age == ag & sex == sx)
    
    if (nrow(sub_df) >= 2 && all(sub_df$val > 0)) {
      sub_df$y <- log(sub_df$val)
      model <- lm(y ~ year, data = sub_df)
      beta <- summary(model)[["coefficients"]][2, 1]
      se <- summary(model)[["coefficients"]][2, 2]
      
      EAPC <- 100 * (exp(beta) - 1)
      LCI <- 100 * (exp(beta - 1.96 * se) - 1)
      UCI <- 100 * (exp(beta + 1.96 * se) - 1)
      CI <- paste0(round(EAPC, 2), " (", round(LCI, 2), "-", round(UCI, 2), ")")
      
      result <- rbind(result, data.frame(
        location = loc,
        age = ag,
        sex = sx,
        EAPC = round(EAPC, 2),
        LCI = round(LCI, 2),
        UCI = round(UCI, 2),
        CI = CI,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(result)
}
Table1_7 <- gallbladder %>% filter(measure == "Incidence",
                                             metric == "Rate",
                                             age == "Age-standardized")

Table1_7 <- calculate_EAPC_forloop(Table1_7)
Table1_7 <- Table1_7[,-c(2,4,5,6)]
Table1_7 <- Table1_7%>%pivot_wider(names_from = sex,
                                   values_from = CI)

#重命名一下
Table1_incidence_1 <- Table1_1
Table1_incidence_234 <- Table1_234
Table1_incidence_56 <- Table1_56
Table1_incidence_7 <- Table1_7

#然后再把各个列名重命名
Table1_incidence_1 <- Table1_incidence_1 %>% rename(Male_Incidence_Percentage_change_ASRs_1990_2021=Male,
                                                   Female_Incidence_Percentage_change_ASRs_1990_2021=Female,
                                                   Both_Incidence_Percentage_change_ASRs_1990_2021=Both)

Table1_incidence_234 <- Table1_incidence_234 %>% rename(Male_Incidence_Percentagechange_Number_1990_2021=Male,
                                                        Female_Incidence_Percentagechange_Number_1990_2021=Female,
                                                        Both_Incidence_Percentagechange_Number_1990_2021=Both)

Table1_incidence_56 <- Table1_incidence_56 %>% rename(Both_Incidence_ASR_1990=Both_1990,
                                                      Both_Incidence_ASR_2021=Both_2021,
                                                      Male_Incidence_ASR_1990=Male_1990,
                                                      Male_Incidence_ASR_2021=Male_2021,
                                                      Female_Incidence_ASR_1990=Female_1990,
                                                      Female_Incidence_ASR_2021=Female_2021)

Table1_incidence_7 <- Table1_incidence_7 %>%rename(Both_Incidence_Age_standardized_EAPC = Both,
                                                   Male_Incidence_Age_standardized_EAPC=Male,
                                                   Female_Incidence_Age_standardized_EAPC=Female)


Table1_incidence <- cbind(Table1_incidence_1,Table1_incidence_234,Table1_incidence_56,Table1_incidence_7)
Table1_incidence <- Table1_incidence[,-c(5,9,16)]
Table1_incidence <- Table1_incidence%>%rename(location=location...1)
#至此，其实table1已经结束了，因为他们只做了incidence部分，但是因为制图的原因，还需要做dalys，deaths和prevalence
#----------------------------------------------------------------------------------------------------------------------------------------------------------------
#以上，做完了measure的incidence部分，接下来炮制prevalence，dalys，deaths
#deaths


Table1_deaths_1 <- gallbladder%>%filter(measure == "Deaths",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")

Table1_deaths_1 <- Table1_deaths_1[,c(2,3,7,8)]

Table1_deaths_1 <- Table1_deaths_1%>%group_by(location, sex,year) %>%
  pivot_wider(
    names_from = year,
    values_from = val,
    values_fn = mean 
  )
Table1_deaths_1$per_change <- round((Table1_deaths_1$`2021`-Table1_deaths_1$`1990`)/Table1_deaths_1$`1990`*100,3)
Table1_deaths_1 <- Table1_deaths_1[,-c(3,4)]
Table1_deaths_1 <- Table1_deaths_1%>%pivot_wider(names_from = sex,
                                                 values_from = per_change)
#至此写好了Both/Male/Female Deaths_ Percentage change in ASRs from 1990 to 2021
#-------------------------------------------------------------------------------
#234列及其对应的三种性别
Table1_deaths_234<- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary_all_age.csv")
Table1_deaths_234 <- Table1_deaths_234 %>%filter(year%in%c("1990","2021"),metric == "Number",measure=="Deaths")
Table1_deaths_234 <- Table1_deaths_234[,c(2,3,7,8)]

Table1_deaths_234 <- Table1_deaths_234%>%pivot_wider(names_from = year,
                                                     values_from = val,
                                                     values_fn = mean)

Table1_deaths_234$per_change <- round((Table1_deaths_234$`2021`-Table1_deaths_234$`1990`)/Table1_deaths_234$`1990`*100,3)
Table1_deaths_234 <- Table1_deaths_234[,-c(3,4)]
Table1_deaths_234 <- Table1_deaths_234%>%pivot_wider(names_from = sex,
                                                     values_from = per_change)

#-------------------------------------------------------------------------------
#5,6列
Table1_deaths_56 <- gallbladder%>%filter(measure == "Deaths",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")
Table1_deaths_56 <- Table1_deaths_56[,c(2,3,7,8)]
Table1_deaths_56 <- Table1_deaths_56%>%pivot_wider(
  names_from = c(sex, year),
  values_from = val,
  names_sep = "_",      
  values_fn = mean     
)

#-------------------------------------------------------------------------------
#第七列
Table1_deaths_7 <- gallbladder %>% filter(measure == "Deaths",
                                          metric == "Rate",
                                          age == "Age-standardized")

Table1_deaths_7 <- calculate_EAPC_forloop(Table1_deaths_7)
Table1_deaths_7 <- Table1_deaths_7[,-c(2,4,5,6)]
Table1_deaths_7 <- Table1_deaths_7%>%pivot_wider(names_from = sex,
                                                 values_from = CI)

#重命名一下
Table1_deaths_1 <- Table1_deaths_1 %>% rename(Male_Deaths_Percentage_change_ASRs_1990_2021=Male,
                                              Female_Deaths_Percentage_change_ASRs_1990_2021=Female,
                                              Both_Deaths_Percentage_change_ASRs_1990_2021=Both)

Table1_deaths_234 <- Table1_deaths_234 %>% rename(Male_Deaths_Percentagechange_Number_1990_2021=Male,
                                                  Female_Deaths_Percentagechange_Number_1990_2021=Female,
                                                  Both_Deaths_Percentagechange_Number_1990_2021=Both)

Table1_deaths_56 <- Table1_deaths_56 %>% rename(Both_Deaths_ASR_1990=Both_1990,
                                                Both_Deaths_ASR_2021=Both_2021,
                                                Male_Deaths_ASR_1990=Male_1990,
                                                Male_Deaths_ASR_2021=Male_2021,
                                                Female_Deaths_ASR_1990=Female_1990,
                                                Female_Deaths_ASR_2021=Female_2021)

Table1_deaths_7 <- Table1_deaths_7 %>%rename(Both_Deaths_Age_standardized_EAPC = Both,
                                             Male_Deaths_Age_standardized_EAPC=Male,
                                             Female_Deaths_Age_standardized_EAPC=Female)


Table1_deaths <- cbind(Table1_deaths_1,Table1_deaths_234,Table1_deaths_56,Table1_deaths_7)
Table1_deaths <- Table1_deaths[,-c(5,9,16)]
Table1_deaths <- Table1_deaths%>%rename(location=location...1)

#-------------------------------------------------------------------------------
#prevalence

Table1_prevalence_1 <- gallbladder%>%filter(measure == "Prevalence",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")

Table1_prevalence_1 <- Table1_prevalence_1[,c(2,3,7,8)]

Table1_prevalence_1 <- Table1_prevalence_1%>%group_by(location, sex,year) %>%
  pivot_wider(
    names_from = year,
    values_from = val,
    values_fn = mean 
  )
Table1_prevalence_1$per_change <- round((Table1_prevalence_1$`2021`-Table1_prevalence_1$`1990`)/Table1_prevalence_1$`1990`*100,3)
Table1_prevalence_1 <- Table1_prevalence_1[,-c(3,4)]
Table1_prevalence_1 <- Table1_prevalence_1%>%pivot_wider(names_from = sex,
                                                         values_from = per_change)
#-------------------------------------------------------------------------------
Table1_prevalence_234<- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary_all_age.csv")
Table1_prevalence_234 <- Table1_prevalence_234 %>%filter(year%in%c("1990","2021"),metric == "Number",measure=="Prevalence")
Table1_prevalence_234 <- Table1_prevalence_234[,c(2,3,7,8)]

Table1_prevalence_234 <- Table1_prevalence_234%>%pivot_wider(names_from = year,
                                                             values_from = val,
                                                             values_fn = mean)

Table1_prevalence_234$per_change <- round((Table1_prevalence_234$`2021`-Table1_prevalence_234$`1990`)/Table1_prevalence_234$`1990`*100,3)
Table1_prevalence_234 <- Table1_prevalence_234[,-c(3,4)]
Table1_prevalence_234 <- Table1_prevalence_234%>%pivot_wider(names_from = sex,
                                                             values_from = per_change)

#-------------------------------------------------------------------------------
Table1_prevalence_56 <- gallbladder%>%filter(measure == "Prevalence",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")
Table1_prevalence_56 <- Table1_prevalence_56[,c(2,3,7,8)]
Table1_prevalence_56 <- Table1_prevalence_56%>%pivot_wider(
  names_from = c(sex, year),
  values_from = val,
  names_sep = "_",      
  values_fn = mean     
)

#-------------------------------------------------------------------------------
Table1_prevalence_7 <- gallbladder %>% filter(measure == "Prevalence",
                                              metric == "Rate",
                                              age == "Age-standardized")

Table1_prevalence_7 <- calculate_EAPC_forloop(Table1_prevalence_7)
Table1_prevalence_7 <- Table1_prevalence_7[,-c(2,4,5,6)]
Table1_prevalence_7 <- Table1_prevalence_7%>%pivot_wider(names_from = sex,
                                                         values_from = CI)

#重命名一下
Table1_prevalence_1 <- Table1_prevalence_1 %>% rename(Male_Prevalence_Percentage_change_ASRs_1990_2021=Male,
                                                      Female_Prevalence_Percentage_change_ASRs_1990_2021=Female,
                                                      Both_Prevalence_Percentage_change_ASRs_1990_2021=Both)

Table1_prevalence_234 <- Table1_prevalence_234 %>% rename(Male_Prevalence_Percentagechange_Number_1990_2021=Male,
                                                          Female_Prevalence_Percentagechange_Number_1990_2021=Female,
                                                          Both_Prevalence_Percentagechange_Number_1990_2021=Both)

Table1_prevalence_56 <- Table1_prevalence_56 %>% rename(Both_Prevalence_ASR_1990=Both_1990,
                                                        Both_Prevalence_ASR_2021=Both_2021,
                                                        Male_Prevalence_ASR_1990=Male_1990,
                                                        Male_Prevalence_ASR_2021=Male_2021,
                                                        Female_Prevalence_ASR_1990=Female_1990,
                                                        Female_Prevalence_ASR_2021=Female_2021)

Table1_prevalence_7 <- Table1_prevalence_7 %>%rename(Both_Prevalence_Age_standardized_EAPC = Both,
                                                     Male_Prevalence_Age_standardized_EAPC=Male,
                                                     Female_Prevalence_Age_standardized_EAPC=Female)


Table1_prevalence <- cbind(Table1_prevalence_1,Table1_prevalence_234,Table1_prevalence_56,Table1_prevalence_7)
Table1_prevalence <- Table1_prevalence[,-c(5,9,16)]
Table1_prevalence <- Table1_prevalence%>%rename(location=location...1)

#------------------------------------------------------------------------------------
#dalys

Table1_dalys_1 <- gallbladder%>%filter(measure == "DALYs",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")
Table1_dalys_1 <- Table1_dalys_1[,c(2,3,7,8)]
Table1_dalys_1 <- Table1_dalys_1%>%group_by(location, sex,year) %>%
  pivot_wider(names_from = year, values_from = val, values_fn = mean)
Table1_dalys_1$per_change <- round((Table1_dalys_1$`2021`-Table1_dalys_1$`1990`)/Table1_dalys_1$`1990`*100,3)
Table1_dalys_1 <- Table1_dalys_1[,-c(3,4)]
Table1_dalys_1 <- Table1_dalys_1%>%pivot_wider(names_from = sex, values_from = per_change)

Table1_dalys_234<- read.csv("D:/R Windows/RStudio program/excel/gallbladder and biliary_all_age.csv")
Table1_dalys_234 <- Table1_dalys_234 %>%
  mutate(measure = if_else(str_detect(measure, "DALYs"), "DALYs", measure))
Table1_dalys_234 <- Table1_dalys_234 %>%filter(year%in%c("1990","2021"),metric == "Number",measure=="DALYs")
Table1_dalys_234 <- Table1_dalys_234[,c(2,3,7,8)]
Table1_dalys_234 <- Table1_dalys_234%>%pivot_wider(names_from = year, values_from = val, values_fn = mean)
Table1_dalys_234$per_change <- round((Table1_dalys_234$`2021`-Table1_dalys_234$`1990`)/Table1_dalys_234$`1990`*100,3)
Table1_dalys_234 <- Table1_dalys_234[,-c(3,4)]
Table1_dalys_234 <- Table1_dalys_234%>%pivot_wider(names_from = sex, values_from = per_change)

Table1_dalys_56 <- gallbladder%>%filter(measure == "DALYs",metric == "Rate",year %in% c(1990,2021),age == "Age-standardized")
Table1_dalys_56 <- Table1_dalys_56[,c(2,3,7,8)]
Table1_dalys_56 <- Table1_dalys_56%>%pivot_wider(names_from = c(sex, year), values_from = val, names_sep = "_", values_fn = mean)

Table1_dalys_7 <- gallbladder %>% filter(measure == "DALYs", metric == "Rate", age == "Age-standardized")
Table1_dalys_7 <- calculate_EAPC_forloop(Table1_dalys_7)
Table1_dalys_7 <- Table1_dalys_7[,-c(2,4,5,6)]
Table1_dalys_7 <- Table1_dalys_7%>%pivot_wider(names_from = sex, values_from = CI)

Table1_dalys_1 <- Table1_dalys_1 %>% rename(Male_DALYs_Percentage_change_ASRs_1990_2021=Male,
                                            Female_DALYs_Percentage_change_ASRs_1990_2021=Female,
                                            Both_DALYs_Percentage_change_ASRs_1990_2021=Both)

Table1_dalys_234 <- Table1_dalys_234 %>% rename(Male_DALYs_Percentagechange_Number_1990_2021=Male,
                                                Female_DALYs_Percentagechange_Number_1990_2021=Female,
                                                Both_DALYs_Percentagechange_Number_1990_2021=Both)

Table1_dalys_56 <- Table1_dalys_56 %>% rename(Both_DALYs_ASR_1990=Both_1990,
                                              Both_DALYs_ASR_2021=Both_2021,
                                              Male_DALYs_ASR_1990=Male_1990,
                                              Male_DALYs_ASR_2021=Male_2021,
                                              Female_DALYs_ASR_1990=Female_1990,
                                              Female_DALYs_ASR_2021=Female_2021)

Table1_dalys_7 <- Table1_dalys_7 %>% rename(Both_DALYs_Age_standardized_EAPC = Both,
                                            Male_DALYs_Age_standardized_EAPC=Male,
                                            Female_DALYs_Age_standardized_EAPC=Female)

Table1_dalys <- cbind(Table1_dalys_1,Table1_dalys_234,Table1_dalys_56,Table1_dalys_7)
Table1_dalys <- Table1_dalys[,-c(5,9,16)]
Table1_dalys <- Table1_dalys %>% rename(location=location...1)

#-------------------------------------------------------------------------------
#fig4
#这和之前的数据不一样，需要用到全年龄
library(ggheatmap)
Fig4_a <- gallbladder%>%filter(location %in% c("Global","High SDI","High-middle SDI",
                                               "Middle SDI","Low-middle SDI","Low SDI"),
                               !age %in% c("Age-standardized","5-9 years"),measure=="Incidence"
                               ,metric=="Rate",
                               sex=="Both")
#自定义颜色
breaks <- c(32.7,115.5,286.3,422.9,573.1,780.1,1511.8,1922.3,2217.1,2694.6,3494.4)
labels <- c("32.7,115.5","115.5,286.3","286.3,422.9","422.9,573.1","573.1,780.1","780.1,1511.8","1511.8,1922.3","1922.3,2217.1","2217.1,2694.6","2694.6,3494.4")

Fig4_a <- Fig4_a%>%
  mutate(val_group = cut(val,
                         breaks = breaks,
                         include.lowest = TRUE,
                         labels = labels))



my_colors <- c(
  "32.7,115.5"="#003300",
  "115.5,286.3"="#009933",
  "286.3,422.9"="#33cc33",
  "422.9,573.1"="#66ff66",
  "573.1,780.1"="#99ff99",
  "780.1,1511.8"="#ff99ff",
  "1511.8,1922.3"="#ff66ff",
  "1922.3,2217.1"="#ff00ff",
  "2217.1,2694.6"="#cc00cc",
  "2694.6,3494.4"="#660066"
)


ggplot(Fig4_a,mapping = aes(x=year,y=age,fill = val_group))+
  geom_tile()+
  facet_wrap(~location,ncol=6)+
  scale_fill_manual(values = my_colors)+
  theme_minimal()
  


















