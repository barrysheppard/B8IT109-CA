###############################################################################
# Description     : CA for B8IT109 Advanced Data Analytics
# Lecturer        : Dr Shahram Azizi
# Author          : Barry Sheppard - Student Number 10387786
# Date            : 2019/08/18
# Notes           : Question 3
###############################################################################

#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load('quantmod', 'xts', 'ggplot2', 'tseries', 'forecast')

# Question 3
# Using Yahoo Finance API, select a specific stock market price,
# apply time series analysis, consider ‘close price as your time
# series variable:

# (a) Validate the assumptions using graphical visualization.
# (b) Fit the optimized model for ‘close price’ and provide
#     the coefficient estimates for the fitted model.
# (c) What is the estimated order for AR and MA?
# (d) Forecast h=10 step ahead prediction of wage on the plot
#     of the original time series.

# Code to read data from Yahoo Finance from
# https://lamfo-unb.github.io/2017/07/22/intro-stock-analysis-1/

# Other time series code from this guide
# https://rpubs.com/JSHAH/481706

# First we need to fetch the data from Yahoo Finance

data <- getSymbols("GAW.L", src = "yahoo",
                  from = "2012-01-01", to = "2019-07-11",
                  auto.assign = FALSE)

head(data)
dim(data)
str(data)

# Remove all but the closing price
stock_prices <- data[,4]
# check for any NAs
stock_prices[is.na(stock_prices)]

# (a) Validate the assumptions using graphical visualization.

# plot the data
plot(stock_prices, type = 'l', main = 'log returns plot')
# From the plot we can see the mean and variance is not stationary
# This is especially true when we compare to a normalised version
plot(diff(log(stock_prices)))

# We can also see from the qqnorm plot that it is not normal
qqnorm(stock_prices, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE) 


fit <- stl(stock_prices, s.window = "periodic") 
# This generates an error due to the lack of periods as its daily
periodicity(stock_prices)
# Daily periodicity from 2012-01-03 to 2019-07-11 
# Adjust it to have yearly frequency, there are ~253 trading days in the year
yearly_stock_prices <- ts(as.numeric(stock_prices), frequency = 253)
fit <- stl(yearly_stock_prices, s.window = "periodic", robust = TRUE) 
plot(fit)
# based on this we can there is a seasonal trend

# (b) Fit the optimized model for ‘close price’ and provide
#     the coefficient estimates for the fitted model.

auto.fit <- auto.arima(stock_prices, seasonal = TRUE)
auto.fit

# Series: stock_prices 
# ARIMA(5,2,0) 
# 
# Coefficients:
#   ar1      ar2      ar3      ar4      ar5
# -0.8656  -0.6400  -0.4687  -0.3844  -0.2153
# s.e.   0.0224   0.0286   0.0303   0.0286   0.0224
# 
# sigma^2 estimated as 1435:  log likelihood=-9594.14
# AIC=19200.27   AICc=19200.32   BIC=19233.57

# auto.fit <- auto.arima(stock_prices, seasonal = FALSE)
# auto.fit
# 
# Series: stock_prices 
# ARIMA(5,2,0) 
# 
# Coefficients:
#   ar1      ar2      ar3      ar4      ar5
# -0.8656  -0.6400  -0.4687  -0.3844  -0.2153
# s.e.   0.0224   0.0286   0.0303   0.0286   0.0224
# 
# sigma^2 estimated as 1435:  log likelihood=-9594.14
# AIC=19200.27   AICc=19200.32   BIC=19233.57

# As seasonal false is the same as seasonal true, we use False
# The coeffecient estimates are

# Coefficients:
#           ar1      ar2      ar3      ar4      ar5
#       -0.8656  -0.6400  -0.4687  -0.3844  -0.2153
# s.e.   0.0224   0.0286   0.0303   0.0286   0.0224

# ar1 = -0.8656
# ar2 = -0.6400
# ar3 = -0.3844 
# ar4 = -0.2153


# (c) What is the estimated order for AR and MA?

# The estimated order is ARIMA(5,2,0) 
# p = 5
# d = 2
# q = 0


# (d) Forecast h=10 step ahead prediction of close price on the plot
#     of the original time series.

auto.fcast <- forecast(auto.fit, h =  10)
plot(auto.fcast)

