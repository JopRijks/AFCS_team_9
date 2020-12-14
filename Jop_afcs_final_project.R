library(fpp2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)

setwd("./Data")

# read in all the data
calendar <- read_csv("calendar_afcs2020.csv")
sales_train <- read_csv("sales_train_validation_afcs2020.csv")
price <- data.frame(read_csv("sell_prices_afcs2020.csv"))


# extra_data
sales_train_eva <- read_csv("sales_train_evaluation_afcs2020.csv")
sample_submission_afcs2020 <- read_csv("sample_submission_afcs2020.csv")


num = 1

s_train <- t(sales_train[num,])
name = s_train[1,] 
s_train <- data.frame(s_train[-1,])
colnames(s_train) = "product" #name

train <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
train$d <- as.integer(gsub('d_', '', train$Row.names))
#train$date <- as.Date(train$date , format = "%m/%d/%y")

name <- substr(as.character(name), start = 1, stop = 13)
price_select <- price
price_select <- filter(price_select, item_id == name)
train_f <- merge.data.frame(train, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")

for(num in c(0:149)){
  
  
  
}



dt = melt
# 28 days forecasting
h = 28 

sales_train <- t(sales_train)


