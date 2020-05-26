
#Holt-Winters Exponential Smoothing Example

library(forecast)
library(fma)
library(datasets)
library(ggplot2)

#############################
#Simple exponential Smoothing
#############################
airpass

autoplot(airpass)

ses5 <- ses(airpass, h=5)
ses5

accuracy(ses5)

autoplot(ses5) +
autolayer(fitted(ses5),series = "Fitted")

#############################
# Holt's Linear Trend Method
#############################

holt5 <- holt(airpass,h=5)
autoplot(holt5) + 
  autolayer(fitted(holt5),series = "Fitted")

holt5damped <- holt(airpass, damped=TRUE, phi = 0.9, h=15)

#Evidence suggested that the Holt's Linear Trend method 
#overestimated the predicted values. Gardner and McKenzie (1985) 
#found that dampening the trend helped accuracy

autoplot(airpass) +
  autolayer(holt5, series="Holt's method", PI=FALSE) +
  autolayer(holt5damped, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + 
  guides(colour=guide_legend(title="Forecast"))

##############################
# Holt's Seasonal Trend Method
##############################

hw1 <-hw(airpass, seasonal = "additive")
hw2 <-hw(airpass, seasonal = "multiplicative")

autoplot(airpass) +
  autolayer(hw1, series="HW additive forecasts", PI=FALSE) +
  autolayer(hw2, series="HW multiplicative forecasts",
            PI=FALSE) +
  #ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))



