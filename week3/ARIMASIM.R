library(fma)

#ARIMA - White noise model
arima_wn <- arima.sim(model = list(order = c(0,0,0)), n = 100, mean = 0)
arima_wn
ts.plot(arima_wn)

#ARIMA - Random Walk
arima_rw <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean = 0)
arima_rw
ts.plot(arima_rw)

#ARIMA - Random Walk with Drift
arima_rwd <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean = 1)
arima_rwd
ts.plot(arima_rwd)

#ARIMA - First Order Auto-Regressive Model
arima_fo <- arima.sim(model = list(order = c(1,0,0), ar = 0.75), n = 100, mean = 0)
arima_fo
ts.plot(arima_fo)

arima(arima_fo,order=c(1,0,0))

#ARIMA - Moving Average Model
arima_ma <- arima.sim(model =(list(order = c(0,0,1), ma = 0.6)), n = 100, mean = 0)
arima_ma
ts.plot(arima_ma)


#Auto ARIMA

auto.arima(beer)
