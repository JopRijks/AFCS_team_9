library(fpp2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)

# move to data directory for loading the data
setwd("./Data")

# read in all the data
calendar <- read_csv("calendar_afcs2020.csv")
sales_train <- read_csv("sales_train_validation_afcs2020.csv")
price <- data.frame(read_csv("sell_prices_afcs2020.csv"))
sample_org <- data.frame(read_csv("sample_submission_afcs2020.csv"))

# extra_data
true_future <- data.frame(read_csv("sales_train_evaluation_afcs2020.csv"))


# move back to original working directory
setwd("..")

# could be used in future to calculate rmse right away
# if you loop over this with [num,] you get the wanted row now only compare to the forecast with for example accuracy
# ["rmse"] behind it you get rmse and i don't know if you can add/multiplicatice the rmse of all rows or for example compare whole dataset.
future <- true_future[,c(1914:1942)]

# set forecast days
h=28

################################################################
####### moving average model  --- Kaggle score : 0.89757 #######
################################################################

# set sample for the output
sample <- sample_org
for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # combine the data with calendar data
  sales_days <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
  sales_days$d <- as.integer(gsub('d_', '', sales_days$Row.names))
  
  # combine the data with sale price
  price_select <- price
  price_select <- filter(price_select, item_id == name)
  train_draft <- merge.data.frame(sales_days, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  
  # select all the wanted columns for the forecast
  train <- select(train_draft, sales, d ,sell_price, wday, event_name_1, event_type_1, snap_CA)
  
  # forecast model could be changed to wanted model
  xreg <- as.numeric(train$sell_price) # not used right now
  model <- arima(as.numeric(train$sales), order=c(0,0,1))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}

# write csv file for submition
write.csv(sample, "./output/moving_average_R.csv", row.names = F)


################################################################
##### dynamic regression model  --- Kaggle score : 0.????? #####
################################################################

# set sample for the output
sample <- sample_org
for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # combine the data with calendar data
  sales_days <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
  sales_days$d <- as.integer(gsub('d_', '', sales_days$Row.names))
  
  # combine the data with sale price
  price_select <- price
  price_select <- filter(price_select, item_id == name)
  train_draft <- merge.data.frame(sales_days, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  
  #xreg future 
  x_help <- merge.data.frame(calendar, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  x_help$d <- as.integer(gsub('d_', '', x_help$d))
  x_help_f <- filter(x_help, d > 1941) 
  x_reg_f <- as.numeric(x_help_f$sell_price)
  
  # select all the wanted columns for the forecast
  train <- select(train_draft, sales, sell_price)
  
  # forecast model could be changed to wanted model
  xreg <- as.numeric(train$sell_price)# not used right now
  data <- as.numeric(train$sales)
  model <- auto.arima(data, xreg = xreg)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}

# write csv file for submition
write.csv(sample, "./output/moving_average_R.csv", row.names = F)


# select from train file only the selected row
s_train <- t(sales_train[num,])
name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
s_train <- data.frame(s_train[-1,])
colnames(s_train) = "sales" #name

# combine the data with calendar data
sales_days <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
sales_days$d <- as.integer(gsub('d_', '', sales_days$Row.names))

# combine the data with sale price
price_select <- price
price_select <- filter(price_select, item_id == name)
train_draft <- merge.data.frame(sales_days, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")

#xreg future 
x_help <- merge.data.frame(calendar, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
x_help$d <- as.integer(gsub('d_', '', x_help$d))
x_help_f <- filter(x_help, d > 1941) 
x_reg_sell <- as.list(as.numeric(x_help_f$sell_price))
x_week <- model.matrix(~0+x_help_f[,'weekday'])
  
# select all the wanted columns for the forecast
train <- select(train_draft, sales, sell_price, weekday)
train_weekdays <- model.matrix(~0+train[,'weekday'])

# xreg
xreg = cbind(as.numeric(train$sell_price),
             train_weekdays[,1],
             train_weekdays[,2],
             train_weekdays[,3],
             train_weekdays[,4],
             train_weekdays[,5],
             train_weekdays[,6],
             train_weekdays[,7])

xregf = cbind(
  rep(x_reg_sell,1),
  rep(x_week[,1],1),
  rep(x_week[,2],1),
  rep(x_week[,3],1),
  rep(x_week[,4],1),
  rep(x_week[,5],1),
  rep(x_week[,6],1),
  rep(x_week[,7],1)
)
# forecast model could be changed to wanted model

data <- as.numeric(train$sales)
model <- auto.arima(data, xreg = xreg)
model
fcast <- forecast(model, xreg = xregf)
autoplot(fcast)
# create wanted output format
row <- as.numeric(t(data.frame(fcast))[1,])
new_row <- c(sample[num,1], row)
sample[num,] <- new_row