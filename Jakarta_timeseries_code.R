# FORECASTING THE NUMBER OF PATIENTS FOR AMBULANCE SERVICES IN JAKARTA 

# Importing libraries
library(ggplot2)
library(ggthemes)
library(tseries)
library(forecast)
library(dplyr)
library(tidyverse)
library(tidyr)
library(caret)
library(lubridate)
library(timetk)
library(modeltime)
library(fpp2)
library(fpp3)
library(DT)
library(scales)
library(stats)
library(readxl)
library(zoo)

# Importing data
data <- read_excel("... TimeSeriesData.xlsx", sheet = "Data_daily")

# Checking the data for missing values and outliers 
colSums(is.na(data)) # No missing values were found in the dataset
boxplot(data$volume) # There is a significant number of outliers in the dataset

# Creating the time series object
ts_df <- ts(data$volume, start = c(2015, 91), frequency = 365)

# Plotting the time series
plot(ts_df, ylab="Number of patients", main="Time series plot of the daily number of patients")

# Summary statistics
summary(ts_df)
sd(ts_df)


# TIME SERIES 

# Time series decomposition
decomp_ts <- decompose(ts_df)
plot(decomp_ts)

# Dealing with the outliers
boxplot(ts_df) 
boxplot_result <- boxplot(ts_df)
num_outliers <- length(boxplot_result$out)

# Printing the number of outliers
print(num_outliers) 

# Creating a function to replace outliers with NA values using box plot method
remove_outliers_boxplot <- function(data, coef = 1.5) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - coef * IQR
  upper_bound <- Q3 + coef * IQR
  
  data[data < lower_bound | data > upper_bound] <- NA
  return(data)
}

# Replacing outliers with NA values
ts_df <- remove_outliers_boxplot(ts_df)

# Last observation carried forward (LOCF)
# filling in NA values with the most recent non-NA value that it can find.
ts_df <- na.locf(ts_df)

print(ts_df)
boxplot(ts_df)

# Using Augmented Dickey-Fuller test for stationarity
adf_result <- adf.test(ts_df)
adf_result
print(adf_result$p.value)
# The null hypothesis of time series being non-stationary is rejected
# and the alternative hypothesis of stationarity is accepted

# Seasonal plots
ggseasonplot(ts_df, year.labels=TRUE) + 
  xlab("Date") + 
  ylab("Number of patients") +  
  ggtitle("Seasonal plot of the daily number of patients each year") +
  theme(plot.title = element_text(face = "bold"))
# The seasonality plot shows strong seasonality in the time series, 
# particularly around July. However, determining the exact seasonal period 
# from the seasonal plot alone proves challenging. Therefore, we will use 
# the decomposition method for a more precise understanding of seasonality.


# Additive vs multiplicative decomposition
# Additive
ts_df_additive <- decompose(ts_df, type = "additive")
plot(ts_df_additive, col="darkred", lwd=1)

# Multiplicative
ts_df_multiplicative <- decompose(ts_df, type = "multiplicative")
plot(ts_df_multiplicative, col="blue",lwd=1)

# Visual examination of both additive and multiplicative decompositions reveals 
# a consistent seasonal component across them, implying the seasonal component 
# is constant and is independent of the time series and,
# therefore, an additive decomposition model will be used for our analysis.


# Plotting ACF and PACF
Acf(ts_df,lag.max = 50)
Acf(ts_df,lag.max = 50, type = "partial")
# ACF plot - autocorrelation up to lag 50
# PACF plot - significant partial autocorrelation up to lag 4
# The ACF and PACF plots also depict seasonality within the time series, which 
# peaks at lag 7, lag 14, lag 21 and so on. 


# FORECASTING

# First, we split the data into the training (70%) and test (30%) sets

# Training set with 70% of the observations
ts_df_train <- ts(ts_df[1:1064], start = c(2015, 91), frequency = 365)

# Test set with 30% of the observations
ts_df_test <- ts(ts_df[1064:1521], start = c(2018, 59), frequency = 365)  

# Plotting the training and test data
plot(ts_df_train, type = "l", col = 2, xlim = c(2015, 2020), xlab = "Date", ylab = "Number of patients")
lines(ts_df_test, type = "l", col = 3)
legend("bottomleft", c("Train 70%", "Test 30%"), lty = 1, col = 2:4)
title(main = "Train vs test data")

plot(ts_df)
lines(ts_df_train, col = "blue")
lines(ts_df_test, col = "red")


# MODELLING 

# Naive methods

# 1. Forecasting using Naive Model and evaluating its performance
naive_method <- naive(ts_df_train, h=length(ts_df_test))
naive_forecast<- forecast::forecast(naive_method, h=length(ts_df_test))
naive_accuracy <- forecast::accuracy(naive_method, ts_df_test)
naive_accuracy

plot(naive_forecast, xlab = "Date", ylab = "Number of patients", main = "Naive Model Forecasting",
     col = "darkred")
lines(ts_df_test)

# Naive model plot with the legend
plot(naive_forecast, xlab = "Date", ylab = "Number of patients", main = "Naive Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)

# 2. Forecasting using Seasonal Naive Model and evaluating its performance
snaive_method <- snaive(ts_df_train, h=length(ts_df_test))
snaive_forecast<- forecast::forecast(snaive_method, h=length(ts_df_test))
snaive_accuracy <- forecast::accuracy(snaive_forecast, ts_df_test)
snaive_accuracy

plot(snaive_forecast, xlab = "Date", ylab = "Number of patients", main = "Snaive Model Forecasting",
     col = "darkred")
lines(ts_df_test)

# Snaive model plot with the legend
plot(snaive_forecast, xlab = "Date", ylab = "Number of patients", main = "Snaive Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)


# 3.Forecasting using Mean model and evaluating its performance
mean_model<- meanf(ts_df_train, h=length(ts_df_test))
mean_forecast<- forecast::forecast(mean_model, h=length(ts_df_test))
mean_accuracy<-forecast::accuracy(mean_forecast$mean, ts_df_test)
mean_accuracy

plot(mean_forecast, xlab = "Date", ylab = "Number of patients", main = "Mean Model Forecasting",
     col = "darkred")
lines(ts_df_test)

# Mean model plot with the legend
plot(mean_forecast, xlab = "Date", ylab = "Number of patients", main = "Mean Model Forecasting",
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1, cex = 0.8)


# Naive, Snaive, Mean Models visualisations
autoplot(ts_df_train) +
  autolayer(naive_method, series = "Naive Method Forecasting", col="darkred") +
  ggtitle("Naive Method Forecasting") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))

# coordinates adjusted
autoplot(ts_df_train) +
  autolayer(naive_method, series = "Naive Method Forecasting", col="yellow") +
  ggtitle("Naive Method Forecasting") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-300, 300)) # it is better without adjusting the y 

autoplot(ts_df_train) +
  autolayer(mean_model, series = "Mean Model Forecasting", col="darkred") +
  ggtitle("Mean Model Forecasting") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))

autoplot(ts_df_train) +
  autolayer(snaive_method, series = "Snaive Method Forecasting" ,col="darkred") +
  ggtitle("Snaive Method Forecasting") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))

# Naive method, Seasonal Naive method, Mean model visualisations vol.2
autoplot(ts_df_train) +
  autolayer(naive_method, series = "Naive method forecasting") +
  autolayer(mean_model, series = "Mean model forecasting")+
  autolayer(snaive_method, series = "Snaive method forecasting") +
  ggtitle("Naive, Snaive and Mean models forecasting")+
  theme_classic() + xlab("Time") + ylab("Number of patients") + # Time? or Date?
  theme(legend.position = "bottom", legend.box = "horizontal")

plot(ts_df, col="black", xlab="Date",ylab="Number of patients", type='l') 
lines(snaive_forecast$mean, col="blue", lwd=2)
lines(naive_method$mean, col="yellow", lwd=2) 
lines(mean_forecast$mean,col="red",lwd=2)
legend("topleft", lty=1, col=c("yellow","blue","red"), 
       legend=c("Naive Method", "Seasonal Naive Method","Mean Model"))


# Exponential Smoothing Methods

# 1. Forecasting using Simple Exponential Smoothing (SES) Model and evaluating its performance
ses <- ses(ts_df_train, h=length(ts_df_test))
ses_accuracy<-forecast::accuracy(ses, ts_df_test)
ses_accuracy

plot(ses, xlab = "Date", ylab = "Number of patients", main = "SES Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "darkblue")

# SES model plot with the legend
plot(ses, xlab = "Date", ylab = "Number of patients", main = "SES Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)

# 2. Forecasting using Holt's Linear Model and evaluating its performance
holt_lin <- holt(ts_df_train, h = length(ts_df_test))
holt_lin_accuracy<-forecast::accuracy(holt_lin, ts_df_test)
holt_lin_accuracy

plot(holt_lin, xlab = "Date", ylab = "Number of patients", main = "Holt's Linear Model Forecasting", 
     col = "darkred")
lines(ts_df_test)

# Holt's Linear Model plot with the legend
plot(holt_lin, xlab = "Date", ylab = "Number of patients", main = "Holt's Linear Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)

# 3. Forecasting using Holt-Winters' Model and evaluating its performance
holt_wint <- HoltWinters(ts_df_train)
holt_wint_forecast <- forecast::forecast(holt_wint, h = length(ts_df_test))
holt_wint_accuracy <- forecast::accuracy(holt_wint_forecast, ts_df_test)
holt_wint_accuracy

plot(holt_wint_forecast, xlab = "Date", ylab = "Number of patients", main = "Holt-Winters' Model Forecasting", 
     col = "darkred")
lines(ts_df_test)

# Holt-Winters' Model plot with the legend
plot(holt_wint_forecast, xlab = "Date", ylab = "Number of patients", main = "Holt-Winters' Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)


# Regression

# 1. Forecasting using Simple Linear Regression (SLR) Model and evaluating its performance
# trend only

ts_df_trend <-  data.frame(trend = time(ts_df_train))
lin_reg <- lm(ts_df_train ~ ts_df_trend$trend)
ts_df_trend <-  data.frame(trend = time(ts_df_test))
test_pred <- forecast(lin_reg, newdata = ts_df_trend$trend) 
lin_reg_accuracy <- forecast::accuracy(test_pred$mean, ts_df_test)
lin_reg_accuracy

lin_reg_pred <- ts(test_pred$mean, start = c(2018, 59), frequency = 365)

plot(ts_df, xlab = "Date", ylab = "Number of patients", main = "Simple Linear Regression Forecasting", 
     col = "darkred")
lines(lin_reg_pred, col = "blue", lty = 1)

# Simple Linear Regression Model plot with the legend
plot(ts_df, xlab = "Date", ylab = "Number of patients", main = "Simple Linear Regression Forecasting", 
     col = "darkred")
lines(lin_reg_pred, col = "yellow", lty = 1)
legend("topleft", legend=c("Data", "Forecasted values"), 
       col=c("darkred", "yellow"), lty = 1)

# Simple Linear Regression Model plot with the legend (train vs test sets visually separated)
plot(ts_df, xlab = "Date", ylab = "Number of patients", main = "Simple Linear Regression Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
lines(lin_reg_pred, col = "blue", lty = 1)
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "blue"), lty = 1)


# 2. Forecasting using Multiple Linear Regression (MLR) Model and evaluating its performance
# trend + seasonality

lin <- tslm(ts_df_train ~ trend + season)
lin <- forecast::forecast(lin, h = length(ts_df_test))

MLR_accuracy <- forecast::accuracy(lin, ts_df_test)
MLR_accuracy

plot(lin, xlab = "Date", ylab = "Number of patients", main = "MLR Model Forecasting", 
     col = "darkred")
lines(ts_df_test)

# MLR Model plot with the legend
plot(lin, xlab = "Date", ylab = "Number of patients", main = "MLR Model Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow", lty = 1)
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)


# ARIMA Models

# 1. Forecasting using Auto-ARIMA Model and evaluating its performance and (autocorrelation ???)
auto_arima <- auto.arima(ts_df_train, seasonal = TRUE) #ARIMA(3,1,1) is chosen 
summary(auto_arima)
auto_arima_forecast <- forecast::forecast(auto_arima, h = length(ts_df_test))  
auto_arima_accuracy <- forecast::accuracy(auto_arima_forecast, ts_df_test)
auto_arima_accuracy

autoplot(ts_df_train) +
  autolayer(auto_arima_forecast, series = "Auto ARIMA Forecasting") +
  autolayer(ts_df_test, series = "Actual Data") + xlab("Time") +  # Time? or Date?
  ylab("Data") + theme_light()+
  ggtitle("Auto ARIMA Forecasting")

# 2. Forecasting using ARIMA Model and evaluating its performance

# Since the ACF plot decays geometrically but the PACF plot cuts off after lag 4,
# we can use AR(3), AR(4) and AR(5) models and then find the one with the best performance  

arima310 <- Arima(ts_df_train, order = c(3, 1, 0)) 
arima_forecast310 <- forecast::forecast(arima310, h = length(ts_df_test))  
arima_accuracy310 <- forecast::accuracy(arima_forecast310, ts_df_test)
arima_accuracy310

arima410 <- Arima(ts_df_train, order = c(4, 1, 0)) 
arima_forecast410 <- forecast::forecast(arima410, h = length(ts_df_test))  
arima_accuracy410 <- forecast::accuracy(arima_forecast410, ts_df_test)
arima_accuracy410

arima510 <- Arima(ts_df_train, order = c(5, 1, 0)) 
arima_forecast510 <- forecast::forecast(arima510, h = length(ts_df_test))  
arima_accuracy510 <- forecast::accuracy(arima_forecast510, ts_df_test)
arima_accuracy510

# ARIMA(3,1,0) performs the best since it had the lowest RMSE of 11.840754

autoplot(ts_df_train) +
  autolayer(arima_forecast310, series = "AR(3) Forecasting") +
  autolayer(ts_df_test, series = "Test data") +
  autolayer(ts_df_train, series = "Training data") +
  xlab("Date") + ylab("Number of patients") +
  ggtitle("AR(3) Forecasting") +
  scale_color_manual(values = c("AR(3) Forecasting" = "blue", 
                                "Test data" = "yellow", "Training data" = "darkred"))
# alternative
plot(arima_forecast310, xlab = "Date", ylab = "Number of patients", main = "AR(3) Forecasting", 
     col = "darkred")
lines(ts_df_test, col = "yellow") 
legend("topleft", legend=c("Training data", "Test data", "Forecasted values"), 
       col=c("darkred", "yellow", "lightblue"), lty = 1)


# Moving Average models

# Training
ma7 <- ma(ts_df_train, 7)
ma30 <- ma(ts_df_train, 30)
ma90 <- ma(ts_df_train, 90)
ma120 <- ma(ts_df_train, 120)
ma180 <- ma(ts_df_train, 180)

# Testing
ma_f7 <- forecast::forecast(ma7, h = length(ts_df_test))
ma_f30 <- forecast::forecast(ma30, h = length(ts_df_test))
ma_f90 <- forecast::forecast(ma90, h = length(ts_df_test))
ma_f120 <- forecast::forecast(ma120, h = length(ts_df_test))
ma_f180 <- forecast::forecast(ma180, h = length(ts_df_test))

# Evaluating performance 
ma_acc7 <- forecast::accuracy(ma_f7$mean, ts_df_test)
ma_acc30 <- forecast::accuracy(ma_f30$mean, ts_df_test)
ma_acc90 <- forecast::accuracy(ma_f90$mean, ts_df_test)
ma_acc120 <- forecast::accuracy(ma_f120$mean, ts_df_test)
ma_acc180 <- forecast::accuracy(ma_f180$mean, ts_df_test)

# Plotting
autoplot(ts_df,col="darkred", series = "Number of patients", xlab="Date", main="Moving Average of Daily Number of Patients", type='l') + 
  autolayer(ma7, series = "MA(7)",lwd=1) +
  autolayer(ma30, series = "MA(30)",lwd=1) +
  autolayer(ma90, series = "MA(90)",lwd=1) +
  autolayer(ma120, series = "MA(120)",lwd=1) +
  autolayer(ma180, series = "MA(180)",lwd=1) +
  xlab("Date") + 
  ylab("Number of patients")

# Accuracy of the Moving Average models
ma_acc7
ma_acc30
ma_acc90
ma_acc120
ma_acc180
# The best one is the 7-day one MA(7) because it has the lowest RMSE of 9.571003


# Performance Evaluation
naive_accuracy
snaive_accuracy
mean_accuracy
ses_accuracy
holt_lin_accuracy
holt_wint_accuracy
lin_reg_accuracy
MLR_accuracy
arima_accuracy310
ma_acc7


# The Multiple Linear Regression model with trend and season, the Snaive model, 
# and the Moving Average model MA(7) outperform other models in terms of error statistics. 
# Although all three captured the fluctuations and dynamics of the daily number of ambulance 
# patients effectively, the MLR model demonstrates superior robustness compared to the other two 
# when considering MAE, RMSE and MAPE. With its outstanding error statistics, 
# the MLR model will be used as the preferred model for forecasting the number of ambulance patients 
# for the first week of June 2019.


# FORECASTING FOR THE FIRST WEEK IN JUNE 2019

llin <- tslm(ts_df ~ trend + season)
forecast <- forecast::forecast(llin, h = 7, level=c(95))
day_one <- as.Date("2019-06-01")
days_to_forecast <- seq(from = day_one, by = "day", length.out = 7)
cbind(days_to_forecast,as.data.frame(forecast))

plot(forecast, main = "7 days forecast with MLR")
legend("topleft", legend=c("Data", "7 days forecast"), 
       col=c("black", "blue"), lty = 1)

