install.packages("ggplot2")
install.packages("xts")
install.packages("tseries")
install.packages("forecast")
install.packages("sweep")
install.packages("urca")
install.packages("tibble")
library(xts)
library(tseries)
library(forecast)
library(tibble)
library(ggplot2)
library(sweep)
library(urca)

# get SPY data 
SPY=read.csv("SPY.csv",header = TRUE, sep=",")

# Save the date in a separate identifier as character
dates = as.character(SPY$Date)
# Remove date values from table
SPY$Date = NULL
#Announce time series data
SPY=xts(SPY$Price, as.POSIXct(dates,format="%m/%d/%Y"))
#Plot the data
plot(SPY, col="darkred", main="SPY Price Series from 2007-2019") 

# Stationarity testing
StationarityTest = ur.df(SPY,type="none",selectlags = "AIC")
summary(StationarityTest)
#Stationarity Tesing on first Differences
Stationarity_Diff= ur.df(diff(SPY)[2:dim(SPY)[1],], type = "none", selectlags = "AIC")
summary(Stationarity_Diff)

# Plot the graph on first differences
D.SPY= SPY-lag(SPY)
plot(D.SPY, col="red4", main = "SPY On First Differences from 2007 to 2019")

#ACF and PACF  
ggAcf(D.SPY, lag.max = 10) + theme_bw()
ggPacf(D.SPY, lag.max = 10) + theme_bw()

# Fit the ARIMA Model
ARIMA1 <- Arima(D.SPY, order = c(1, 0, 1))
ARIMA2<-Arima(D.SPY, order = c(1, 0, 2))
ARIMA3<-Arima(D.SPY, order = c(2, 0, 1))
ARIMA4<-Arima(D.SPY, order = c(2, 0, 2))
##Display ARIMA results
summary(ARIMA1)
summary(ARIMA2)
summary(ARIMA3)
summary(ARIMA4)

# Forecasting
# Save the date in a separate identifier as character
dts = as.Date(SPY$Date,format="%m/%d/%Y")
# Remove date values from table
SPY$Date = NULL
#Announce time series data
SPY=xts(SPY$Price, dts)

##Building ARIMA Forecasting Graph
# Forecasting
h = 5
fitted_arima = auto.arima(SPY)
arima_forecast =forecast(fitted_arima, h)
arima_sweep = sw_sweep(arima_forecast)
dates              <- c(index(SPY), index(SPY)[length(SPY)] + 1, index(SPY)[length(SPY)] + 2, index(SPY)[length(SPY)] + 3, index(SPY)[length(SPY)] + 4, index(SPY)[length(SPY)] + 5)
arima_sweep  = add_column(arima_sweep, dates)
# Plotting only the passed 50 days of prices. 
arima_sweep_display<- arima_sweep[(dim(arima_sweep)[1]-50):dim(arima_sweep)[1], ]
# Visualizing the forecast
arima_sweep_display %>%
  ggplot(aes(x = dates, y = value, color = key)) +
  ## Prediction intervals
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  ## Actual & Forecast
  geom_line(size = 1) + 
  #geom_point(size = 2) +
  ## Aesthetics
  theme_bw() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "SPY 5 Day Ahead ARIMA Price Forecast", x = "", y = "")


