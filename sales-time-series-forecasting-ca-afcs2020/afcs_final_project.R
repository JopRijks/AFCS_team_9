library(fpp2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)

# read in all the data
calendar <- read_csv("calendar_afcs2020.csv")
sales_train <- read_csv("sales_train_evaluation_afcs2020.csv")


# extra_data
sales_train_validation_afcs2020 <- read_csv("sales_train_validation_afcs2020.csv")
sample_submission_afcs2020 <- read_csv("sample_submission_afcs2020.csv")
sell_prices_afcs2020 <- read_csv("sell_prices_afcs2020.csv")


# 28 days forecasting
h = 28 

sales_train <- t(sales_train)


