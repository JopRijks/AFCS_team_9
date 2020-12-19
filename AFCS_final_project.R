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
sample <- data.frame(read_csv("sample_submission_afcs2020.csv"))

# extra_data
true_future <- data.frame(read_csv("sales_train_evaluation_afcs2020.csv"))


# move back to original working directory
setwd("..")

# set forecast days
h=28

# moving average model
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


