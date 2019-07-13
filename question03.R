
#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load('quantmod', 'ggplot2')

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

pbr <- getSymbols("GAW.L", src = "yahoo",
                  from = "2018-01-01", to = "2019-07-11",
                  auto.assign = FALSE)

# Remove all but the closing price
ts <- pbr[,4]

head(ts)
describe(ts)
str(ts)

# (a) Validate the assumptions using graphical visualization.

chart_Series(ts)


dts=diff(ts)
plot(dts)
class(dts)

# (b) Fit the optimized model for ‘close price’ and provide
#     the coefficient estimates for the fitted model.

auto.fit <- auto.arima(ts, seasonal=T)  # automatic way to find the optimized model
tsdisplay(residuals(auto.fit), lag.max=45)
summary(auto.fit)

# Series: ts 
# ARIMA(2,1,2) with drift 
# 
# Coefficients:
#   ar1      ar2     ma1     ma2   drift
# -0.2825  -0.5965  0.2727  0.7108  5.8976
# s.e.   0.2328   0.1838  0.2057  0.1600  3.7954
# 
# sigma^2 estimated as 5044:  log likelihood=-2185.08
# AIC=4382.15   AICc=4382.38   BIC=4405.87
# 
# Training set error measures:
#   ME     RMSE      MAE         MPE     MAPE
# Training set 0.04349753 70.46956 51.59043 -0.04900051 1.651401
# MASE       ACF1
# Training set 0.9927355 0.01596298

# (c) What is the estimated order for AR and MA?

AR <- arima(ts, order = c(1,0,0))
print(AR)

# Call:
#   arima(x = ts, order = c(1, 0, 0))
# 
# Coefficients:
#   ar1  intercept
# 0.9974  3403.2758
# s.e.  0.0030   808.9489
# 
# sigma^2 estimated as 5116:  log likelihood = -2198.59,  aic = 4403.19
# > 

MA <- arima(AirPassengers, order = c(0,0,1))
print(MA)

# Call:
#   arima(x = AirPassengers, order = c(0, 0, 1))
# 
# Coefficients:
#   ma1  intercept
# 0.9642   280.6464
# s.e.  0.0214    10.5788
# 
# sigma^2 estimated as 4205:  log likelihood = -806.43,  aic = 1618.86

AIC(AR)
AIC(MA)
BIC(AR)
BIC(MA)

# Based on these scores, MA is lower and probably a better fit

# (d) Forecast h=10 step ahead prediction of wage on the plot
#     of the original time series.

auto.fcast <- forecast(auto.fit, h=10)
plot(auto.fcast)

