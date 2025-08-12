#loess




gallbladder <- read.csv("D:\\R Windows\\RStudio program\\program\\gallbladder复现\\excel\\fig6&fig8\\fig6_country.csv")
gallbladder <- gallbladder %>%
  mutate(measure = if_else(str_detect(measure, "DALYs"), "DALYs", measure))

#-------------------------------------------------------------------------------

#图A
#Incidence rate
data_Incidence <- gallbladder%>%
  filter(metric=="Rate",
         measure=="Incidence",
         age=="Age-standardized",
         sex=="Both")
SDI <- read_xlsx("D:/R Windows/RStudio program/excel/SDI数据.xlsx")
library(janitor)
SDI <- janitor::row_to_names(SDI, row_number = 1)

SDI <- SDI%>%
  pivot_longer(cols = `1990`:`2021`,names_to = "year")%>%
  rename(sdi=value)

colnames(SDI)[1] <- "location"
SDI$year <- as.integer(SDI$year)

data1 <- left_join(data_Incidence,SDI,by=c("location","year"))





ggplot(data1, aes(x = sdi, y = val, color = location)) +
  geom_point(aes(size = year), alpha = 0.6) +
  stat_smooth(aes(group = 1), method = "loess",stat = "smooth",method.args = list(span = 0.3), se = FALSE, color = "black") + 
  scale_size_continuous(range = c(1, 4)) +  # 控制点的最小/最大大小
  guides(size = guide_legend(override.aes = list(size = 3))) +  # 缩小图例中的点
  theme_minimal()



#-------------------------------------------------------------------------------

#图C
#Prevalence rate

data_Prevalence <- gallbladder%>%
  filter(metric=="Rate",
         measure=="Prevalence",
         age=="Age-standardized",
         sex=="Both")
SDI <- read_xlsx("D:/R Windows/RStudio program/excel/SDI数据.xlsx")
library(janitor)
SDI <- janitor::row_to_names(SDI, row_number = 1)

SDI <- SDI%>%
  pivot_longer(cols = `1990`:`2021`,names_to = "year")%>%
  rename(sdi=value)

colnames(SDI)[1] <- "location"
SDI$year <- as.integer(SDI$year)

data3 <- left_join(data_Prevalence,SDI,by=c("location","year"))



ggplot(data3, aes(x = sdi, y = val, color = location)) +
  geom_point(aes(size = year), alpha = 0.6) +
  stat_smooth(aes(group = 1), method = "loess",stat = "smooth",method.args = list(span = 0.3), se = FALSE, color = "black") + 
  scale_size_continuous(range = c(1, 4)) +  # 控制点的最小/最大大小
  guides(size = guide_legend(override.aes = list(size = 3))) +  # 缩小图例中的点
  theme_minimal()




#-------------------------------------------------------------------------------

#图B

data2 <- gallbladder%>%
  filter(metric=="Rate",
         measure=="Incidence",
         age=="Age-standardized",
         sex=="Both",
         year=="2021")

SDI <- read_xlsx("D:/R Windows/RStudio program/excel/SDI数据.xlsx")
library(janitor)
SDI <- janitor::row_to_names(SDI, row_number = 1)

SDI <- SDI%>%
  pivot_longer(cols = `1990`:`2021`,names_to = "year")%>%
  rename(sdi=value)

colnames(SDI)[1] <- "location"
SDI$year <- as.integer(SDI$year)

data2 <- left_join(data2,SDI,by=c("location","year"))


ggplot(data2, aes(x = sdi, y = val, color = location)) +
  geom_point(aes(size = year), alpha = 0.6) +
  geom_smooth(aes(group = 1), method = "loess",stat = "smooth",method.args = list(span = 0.3), se = FALSE, color = "black") + 
  scale_size_continuous(range = c(1, 4)) +  # 控制点的最小/最大大小
  guides(size = guide_legend(override.aes = list(size = 3))) +  # 缩小图例中的点
  theme_minimal()


























































































































