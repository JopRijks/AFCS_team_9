library(fpp2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(Metrics)
library(prophet)
library(reshape2)
library(cowplot)


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
######### Prophet forecast  --- RMSE score : 0.9000961 #########
################################################################
# set sample for the output

sample <- sample_org

for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "value" #name
  lengte <- lengths(s_train)
  
  train <- as.numeric(s_train[,1]) + 1
  
  
  # get data for prophet
  lam = BoxCox.lambda(train, method = "loglik")
  y = BoxCox(train, lam)
  data <- cbind(data.frame(train), data.frame(y))
  
  # combine data and sales
  mydatetime_train <- as.POSIXct("2011-01-29 00:00:00", tz = "UTC")
  dates_train <- seq.POSIXt(from = mydatetime_train, length.out = lengte , by = "1 day")
  data <- cbind(data, data.frame(dates_train))
  
  
  # name columns
  names(data)[1] <- "value"
  names(data)[2] <- "y"
  names(data)[3] <-"ds"
  
  # melt and forecast
  data.m <- melt(data, measure.vars=c("value", "y"))
  m <- prophet(data, daily.seasonality=FALSE)
  future <- make_future_dataframe(m, freq = "day",periods = 28)
  forecast <- predict(m, future)
  
  row <- as.numeric(t(data.frame(forecast$yhat))[1,])
  new_row <- c(sample[num,1], tail(row,28))
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
######### Prophet forecast  --- Kaggle score : 0.94112 #########
################################################################

sales_train <- total

# set sample for the output
sample <- sample_org

for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "value" #name
  lengte <- lengths(s_train)
  
  train <- as.numeric(s_train[,1]) + 1
  
  
  # get data for prophet
  lam = BoxCox.lambda(train, method = "loglik")
  y = BoxCox(train, lam)
  data <- cbind(data.frame(train), data.frame(y))
  
  # combine data and sales
  mydatetime_train <- as.POSIXct("2011-01-29 00:00:00", tz = "UTC")
  dates_train <- seq.POSIXt(from = mydatetime_train, length.out = lengte , by = "1 day")
  data <- cbind(data, data.frame(dates_train))
  
  
  # name columns
  names(data)[1] <- "value"
  names(data)[2] <- "y"
  names(data)[3] <-"ds"
  
  # melt and forecast
  data.m <- melt(data, measure.vars=c("value", "y"))
  m <- prophet(data, daily.seasonality=FALSE)
  future <- make_future_dataframe(m, freq = "day",periods = 28)
  forecast <- predict(m, future)
  
  row <- as.numeric(t(data.frame(forecast$yhat))[1,])
  new_row <- c(sample[num,1], tail(row,28))
  sample[num,] <- new_row
}

write.csv(sample, "./output/Prophet_R.csv", row.names = F)


################################################################
##################### Set Holiday Dates ########################
################################################################
cal <- calendar
cal$date <- as.Date(cal$date, format = '%m/%d/%Y')

snap_ca <- filter(cal, snap_CA == 1)
snap_ca <- as.list(select(snap_ca, date))
snap_ca_df <- data_frame(
  holiday = 'snap_ca',
  ds = as.Date(snap_ca$date),
  lower_window = 0,
  upper_window = 1)

events <- cal[!is.na(cal$event_name_1),]
events <- as.list(select(events, date))
events_df <- data_frame(
  holiday = 'events',
  ds = as.Date(events$date),
  lower_window = 0,
  upper_window = 1
  )

holidays <- bind_rows(snap_ca_df, events_df)


################################################################
######################### Validation ###########################
##### Holiday Prophet forecast  --- RMSE score : 0.8969995 #####
################################################################

# set sample for the output
sample <- sample_org

for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "value" #name
  lengte <- lengths(s_train)
  
  train <- as.numeric(s_train[,1]) + 1
  
  
  # get data for prophet
  lam = BoxCox.lambda(train, method = "loglik")
  y = BoxCox(train, lam)
  data <- cbind(data.frame(train), data.frame(y))
  
  # combine data and sales
  mydatetime_train <- as.POSIXct("2011-01-29 00:00:00", tz = "UTC")
  dates_train <- seq.POSIXt(from = mydatetime_train, length.out = lengte , by = "1 day")
  data <- cbind(data, data.frame(dates_train))
  
  
  # name columns
  names(data)[1] <- "value"
  names(data)[2] <- "y"
  names(data)[3] <-"ds"
  
  # melt and forecast
  data.m <- melt(data, measure.vars=c("value", "y"))
  m <- prophet(data, holidays = holidays, daily.seasonality = FALSE)
  future <- make_future_dataframe(m, freq = "day",periods = 28)
  forecast <- predict(m, future)
  
  row <- as.numeric(t(data.frame(forecast$yhat))[1,])
  new_row <- c(sample[num,1], tail(row,28))
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
##### Holiday Prophet forecast  --- Kaggle score : 0.94164 #####
################################################################

sales_train <- total

# set sample for the output
sample <- sample_org

for(num in c(1:149)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 13)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "value" #name
  lengte <- lengths(s_train)
  
  train <- as.numeric(s_train[,1]) + 1
  
  
  # get data for prophet
  lam = BoxCox.lambda(train, method = "loglik")
  y = BoxCox(train, lam)
  data <- cbind(data.frame(train), data.frame(y))
  
  # combine data and sales
  mydatetime_train <- as.POSIXct("2011-01-29 00:00:00", tz = "UTC")
  dates_train <- seq.POSIXt(from = mydatetime_train, length.out = lengte , by = "1 day")
  data <- cbind(data, data.frame(dates_train))
  
  
  # name columns
  names(data)[1] <- "value"
  names(data)[2] <- "y"
  names(data)[3] <-"ds"
  
  # melt and forecast
  data.m <- melt(data, measure.vars=c("value", "y"))
  m <- prophet(data, holidays = holidays, daily.seasonality = FALSE)
  future <- make_future_dataframe(m, freq = "day",periods = 28)
  forecast <- predict(m, future)
  
  row <- as.numeric(t(data.frame(forecast$yhat))[1,])
  new_row <- c(sample[num,1], tail(row,28))
  sample[num,] <- new_row
}

write.csv(sample, "./output/Prophet_R_Holiday.csv", row.names = F)

