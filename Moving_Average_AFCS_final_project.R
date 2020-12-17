library(fpp2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(Metrics)
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

validation <- true_future[,c(1887:1914)]
future <- true_future[,c(1915:1942)]

# set forecast days
h=28

################################################################
######################### Validation ###########################
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
  train <- select(train_draft, sales)
  train <- head(df, -28)
  
  # forecast model could be changed to wanted model
  model <- arima(as.numeric(train$sales), order=c(0,0,4))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}


rmse_total = c()
for(num in c(1:149)){
  o = as.list(as.data.frame(t(validation[num,])))
  m = as.list(as.data.frame(as.numeric(t(sample[num,2:29]))))
  o <- as.numeric(as.character(unlist(o[[1]])))
  m <- as.numeric(as.character(unlist(m[[1]])))
  rmse_save = rmse(m,o)
  rmse_total <- c(rmse_total, rmse_save)
}
mean(rmse_total)

# write csv file for submition
write.csv(sample, "./output/moving_average_R_validation.csv", row.names = F)

# moving average 1: 0.8694101
# moving average 2: 0.869171
# moving average 3: 0.8689296
# moving average 4: 0.8688903 on kaggle : 0.89689 new
# moving average 5: 0.8690063


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
  train <- select(train_draft, sales)
  
  # forecast model could be changed to wanted model
  model <- arima(as.numeric(train$sales), order=c(0,0,4))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}


rmse_total = c()
for(num in c(1:149)){
  o = as.list(as.data.frame(t(future[num,])))
  m = as.list(as.data.frame(as.numeric(t(sample[num,2:29]))))
  o <- as.numeric(as.character(unlist(o[[1]])))
  m <- as.numeric(as.character(unlist(m[[1]])))
  rmse_save = rmse(m,o)
  rmse_total <- c(rmse_total, rmse_save)
}
mean(rmse_total)

# write csv file for submition
write.csv(sample, "./output/moving_average_R.csv", row.names = F)

# moving average 1: 0.8694101
# moving average 2: 0.869171
# moving average 3: 0.8689296
# moving average 4: 0.8688903 on kaggle : 0.89689 new
# moving average 5: 0.8690063


