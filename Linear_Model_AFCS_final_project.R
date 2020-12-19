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
total <- data.frame(read_csv("sales_train_evaluation_afcs2020.csv"))

# move back to original working directory
setwd("..")


validation <- total[,c(1915:1942)]

# set forecast days
h=28

################################################################
######################### Validation ###########################
########## linear model  --- RMSE score : 1.408583  ############
################################################################

calendar$date <- as.Date(calendar$date, format = '%m/%d/%Y')

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
  train <- select(train_draft, sales, sell_price, snap_CA)
  train <- ts(train)
  
  # validation
  new_data <- merge.data.frame(calendar, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  new_data$d <- as.integer(gsub('d_', '', new_data$d))
  new_data <- filter(new_data, d > 1913)
  new_data <- select(new_data, sell_price, snap_CA)
  new_data$sell_price =  as.numeric(as.character(new_data$sell_price))
  
  # forecast model could be changed to wanted model
  model <- tslm(sales ~ sell_price + snap_CA, data = train)
  fcast <- forecast(model, newdata = new_data)
  
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

################################################################
########## linear model  --- Kaggle score : 0.?????  ###########
################################################################

sales_train <- total

calendar$date <- as.Date(calendar$date, format = '%m/%d/%Y')
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
  train <- select(train_draft, sales, sell_price, snap_CA)
  train <- ts(train)
  
  # validation
  new_data <- merge.data.frame(calendar, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  new_data$d <- as.integer(gsub('d_', '', new_data$d))
  new_data <- filter(new_data, d > 1941)
  new_data <- select(new_data, sell_price, snap_CA)
  new_data$sell_price =  as.numeric(as.character(new_data$sell_price))
  
  # forecast model could be changed to wanted model
  model <- tslm(sales ~ sell_price + snap_CA, data = train)
  fcast <- forecast(model, newdata = new_data)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
# write csv file for submition
write.csv(sample, "./output/linear_model_R.csv", row.names = F)
