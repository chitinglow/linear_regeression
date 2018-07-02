setwd("~/Desktop/learning/Wechat/wechat")

library(readxl)
library(Amelia)
library(dplyr)
library(psych)
library(tidyverse)
library(caret)

#read data
data <- read_xlsx("weather.xlsx", na = "NA")
head(data)

#rename header
colnames(data) <- c("time", "day", "highest_temp", "lowest_temp", "weather", "wind_direction", "wind_speed", "air_quality", "air_pollution")

#recoding header
data$day <- recode(data$day, "星期日" = "Sunday", "星期一" = "Monday", "星期二" = "Tuesday", "星期三" = "Wednesday", "星期四" = "Thursday", "星期五" = 'Friday', "星期六" = "Saturday" )
data$wind_speed <- recode(data$wind_speed, "1-2级" = "Light Breeze", "3-4级" = "Gentle Breeze", "4-5级" = "Moderate Breeze", "5-6级" = "Strong Breeze", "微风" = "Breeze")
data$air_pollution <- recode(data$air_pollution, "严重污染" = "Very Poor", "中度污染" = "Moderate", "优" = "Very Good", "良" = "Good", "轻度污染" = "Light", "重度污染" = "Poor" )
data$weather <- recode(data$weather, "中雨~小雨" = "raining", "多云" = "cloudy", "多云~中雨" = "raining", "多云~小雨" = "raining", "多云~晴" = "cloudy", "多云~阴" = "cloudy", "多云~雷阵雨" = "raining", "多云~雷雨" = "raining", "大雨" = "raining", "小雨~中雨" = "raining",  "小雨~多云" = "raining", "晴" = "sunny", "晴~多云" = "cloudy", "晴~雷阵雨" = "raining", "晴~雷雨" = "raining", "晴~霾" = "fog", "阴" = "cloudy", "阴~小雪" = "cloudy", "阵雨" = "raining", "阵雨~中雨" = "raining", "阵雨~多云" = "raining", "阵雨~晴" = "raining", "阵雨~暴雨" = "raining", "雨夹雪~多云" = "raining", "雷阵雨~多云" = "raining", "霾~多云" = "fog", "霾~晴" = "fog")
data$wind_direction <- recode(data$wind_direction, '东' = "East", "东~北" = "North East", "东北" = "North East", "东北~无持续" = "North East", "东南" = "South East", "东南~无持续" = "South East", "北" = "North",  "北~西北" = "North West",  "南" = "South", "南~东" = "South East",  "南~东北" = "North East",  "南~东南" = "South East",   "南~无持续" = "North", "南~西" = 'North West', "无持续" = "Inconsistent", "无持续~东北" = "North East",  "无持续~东南" = "South East",  "无持续~南" = "South", "无持续~西北" = "North West",  "西" = "West", "西~无持续" = "West", "西北" = "North West", "西北~西" = "North West", "西南" = "South West", "西南~东北" = "South West" ,"西南~无持续" = "South West", "西南~西北" = "West")

#remove ℃ in data 
data$highest_temp = unlist(strsplit(data$highest_temp, split = '℃', fixed = TRUE))
data$lowest_temp = unlist(strsplit(data$lowest_temp, split = '℃', fixed = TRUE))

#checking missing data
missmap(data)

#recode into the relevant data type

data$day <- as.factor(data$day)
data$weather <- as.factor(data$weather)
data$wind_direction <- as.factor(data$wind_direction)
data$wind_speed <- as.factor(data$wind_speed)
data$air_pollution <- as.factor(data$air_pollution)
data$highest_temp <- as.numeric(data$highest_temp)
data$lowest_temp <- as.numeric(data$lowest_temp)

str(data)


#exploratory analysis
##put into descending order
##cheking temperature across the year
data[ order(data$time , decreasing = TRUE ),]
ggplot(data, aes(x = time)) + 
  geom_line(aes(y = highest_temp, color = "red"),size = 1) + 
  geom_line(aes(y = lowest_temp, color = "blue"), size = 1) +
  scale_x_datetime(date_labels = ("%Y-%m-%d"), expand = c(0,0), date_breaks = ("year")) + 
  labs(title = "Temperature from 2012 to 2018", x = "Date", y = "Highest Temperature",  color = "Temperature") +
  scale_color_manual(labels = c("Lowest", "Highest"), values = c("blue", "red")) + 
  theme_bw()

#checking air pollution by day
df <- table(data$day, data$air_pollution)  
df <- as.data.frame(df)
colnames(df) <- c("day", "air_pollution", "Freq")
df$day <- factor(df$day, levels = c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
df[order(df$day), ]

ggplot(df, aes(x = day, y = Freq, fill = air_pollution)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Air Pollution by Week", x = "Day", y = "Frequency") + 
  guides(fill = guide_legend(title = "Air Pollution")) +
  theme_bw()

#checking wind direction and air pollution
df1 <- table(data$wind_direction, data$air_pollution)
df1 <- as.data.frame(df1)
colnames(df1) <- c("wind_direction", "air_pollution", "Freq")

ggplot(df1, aes(x = wind_direction, y = Freq, fill = air_pollution)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Air Pollution by Wind Direction", x = "Wind Direction", y = "Frequency") + 
  guides(fill = guide_legend(title = "Air Pollution")) +
  theme_bw()

#making regression model
missmap(data)
#variance improtant
# ensure results are repeatable
set.seed(7)
# prepare training scheme
#remove missing value and time
names(data)
data.na <- data[,c(-1,-2,-7)]

#making regression model
summary(lm(air_quality ~ ., data = data.na))

anova(lm(air_quality ~ ., data = data.na))
