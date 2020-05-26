
library(forecast)
library(ggplot2)
library(ggfortify)

##### Moving Average ######



# The head() command allows you to take a look at the first few rows of the dataset
head(gold)


# The dim() command allows you to check the size of the dataset
# The first value indicates the number of rows
# The second value indicates the number of columns
str(gold)
dim(gold)

# Next we create gold time series

goldts <-ts(gold[,2])

autoplot(goldts)

##############################################################################
# OPTIONAL - The following adds options to label the chart
##############################################################################

autoplot(goldts) +
  ggtitle("Daily Gold Prices") +
  xlab("period: Jan 1, 1968 to June 11, 2019") +
  ylab("USD")

# The moving average command is ma(x, order)
# x is the data vector and order is the number of lags
# Make sure you install the forecast library

###  example: moving average 10 lags

goldts10lags <- ma(goldts, 10)

autoplot(goldts10lags) +
  ggtitle("Gold Prices: Moving Average 10 lags") +
  xlab("period: Jan 1, 1968 to June 11, 2019") +
  ylab("USD")


goldts100lags <- ma(goldts, 100)

autoplot(goldts100lags) +
  ggtitle("Gold Prices: Moving Average 100 lags") +
  xlab("period: Jan 1, 1968 to June 11, 2019") +
  ylab("USD")

goldts500lags <- ma(goldts, 500)

autoplot(goldts500lags) +
  ggtitle("Gold Prices: Moving Average 500 lags") +
  xlab("period: Jan 1, 1968 to June 11, 2019") +
  ylab("USD")

