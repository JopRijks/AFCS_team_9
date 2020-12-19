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
######### Auto ARIMA model  --- RMSE score : 0.8195932 #########
################################################################

# set sample for the output
sample <- sample_org
for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # select all the wanted columns for the forecast
  train <- select(s_train, sales)
  
  # forecast model could be changed to wanted model
  model <- auto.arima(as.numeric(train$sales))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}

rmse_total = c()
MAE_total = c()
smape_total = c()
for(num in c(1:149)){
  o = as.list(as.data.frame(t(validation[num,])))
  m = as.list(as.data.frame(as.numeric(t(sample[num,2:29]))))
  o <- as.numeric(as.character(unlist(o[[1]])))
  m <- as.numeric(as.character(unlist(m[[1]])))
  rmse_total <- c(rmse_total, rmse(m,o))
  MAE_total <- c(MAE_total, mae(m,o))
  smape_total <- c(smape_total, smape(m,o))
}
mean(rmse_total)
mean(MAE_total)
mean(smape_total)


################################################################
######### Auto ARIMA model  --- Kaggle score : 0.89535 #########
################################################################

sales_train <- total
# set sample for the output
sample <- sample_org
for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # select all the wanted columns for the forecast
  train <- select(s_train, sales)
  
  # forecast model could be changed to wanted model
  model <- auto.arima(as.numeric(train$sales))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
#write csv file for submition
write.csv(sample, "./output/auto_arima_R.csv", row.names = F)

