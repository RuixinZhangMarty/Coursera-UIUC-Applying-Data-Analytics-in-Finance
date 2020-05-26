# install.packages("xts")

library(xts)
library(tseries)
library(quantmod)
library(PerformanceAnalytics)

# Column 6th is the Adjusted Closing price in Yahoo Finance Data
AAPL<-getSymbols("AAPL", source="yahoo", from = "2019-01-01",to = "2019-03-31",
                 auto.assign=FALSE, return.class="xts")[,6]

#Creating time-series for AAPL stock
AAPL <- xts(x=AAPL$AAPL.Adjusted, order.by = index(AAPL))

# Stationarity Testing 
adf = adf.test(AAPL)
print(adf)

# Stationarity testing on first differences

AAPL.diff= diff(AAPL)

AAPL.diff = na.omit(AAPL.diff)

diff.adf =adf.test(AAPL.diff)

print(diff.adf)

# Plot the data

par(mfrow=c(2,1), mar=c(3,4,4,2)) #Code to put both graphs in one window
plot(AAPL, col="darkblue", ylab="Price")
plot(AAPL.diff,col="darkblue", ylab="Price")

