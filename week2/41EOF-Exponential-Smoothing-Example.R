######
###### This code is an example of the simple
###### exponential smoothing method

library(fma)   #library of datasets
library(forecast)



# Load the time series data: beer
# This data set contains the Australian
# monthly beer production from Jan 91 - Aug 95

data(beer)     #loads the time series data beer
 
#lets see the data

beer


#plot of data
autoplot(beer)

# simple exponential smoothing using alpha at .1, .5, and .9
# Which one has the lowest RMSE?

beer1 <- ses(beer,h=25, level=c(80,95), alpha = .1 )
summary(beer1)
autoplot(beer1)

beer5 <- ses(beer,h=25, level=c(80,95), alpha = .5 )
summary(beer5)
autoplot(beer5)

beer9 <- ses(beer,h=25, level=c(80,95), alpha = .9 )
summary(beer9)
autoplot(beer9)

