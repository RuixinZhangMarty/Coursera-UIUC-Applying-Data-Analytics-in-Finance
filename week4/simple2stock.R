#installing and calling the packages
install.packages("quantmod")
install.packages("BatchGetSymbols")
install.packages("timeSeries")
library(quantmod)
library(BatchGetSymbols)
library(timeSeries)


#Obtaining Stock Tickers and Calculating Stock Returns Data
#First, we need to get the tickers of all the stocks in the SnP500 Index

sp500 <- GetSP500Stocks()
sp500tickers <- sp500[,c(1,2,4)]



#Consider 2 stocks 
n = 2
randomstocks <- c(sample.int(500, n))
stocks <- sp500tickers[randomstocks,1]

#Obtain the prices from 'yahoo'
price1 <- getSymbols(stocks[1], source="yahoo", auto.assign=FALSE,
                      return.class="xts")[,6]

price2 <- getSymbols(stocks[2], source="yahoo", auto.assign=FALSE,
                     return.class="xts")[,6]

prices <- cbind(price1, price2)

colnames(prices) <- sp500tickers[randomstocks,2]


#Converting prices to returns
Portfolio <- na.omit(diff(log(prices)))
cor(as.data.frame(Portfolio))

#First we will try to create a set of portfolios using the formulae we learnt in class
#Calculating data parameters for portfolio assets
#Expected Returns
expret1 <- mean(Portfolio[,1])

expret2 <- mean(Portfolio[,2])

#Standard Deviation
sdret1 <- sd(Portfolio[,1])

sdret2 <- sd(Portfolio[,2])

#Covariance
cov_12 <- cov(Portfolio[,1], Portfolio[,2])

#Setting the weights for the 2 asset portfolio
w1 <- (sample(0:100, 100, replace = FALSE))/100
w2 <- 1 - w1

#Calculating data parameters for the portfolio

expret <- w1*expret1 + w2*expret2

sdret <- sqrt(w1^2 * sdret1^2 + w2^2 * sdret2^2 + 2 * w1 * w2 * cov_12)

finalport <- as.data.frame(cbind(sdret, expret))

plot(finalport, ylab = "Expected Return", xlab = "Variance/Risk", main = "Two stock portfolio")


#Now, we will use the fPortfolio package and compare the results with that of the traditional method

library(fPortfolio)

#Let us set the specifications to give us a total of 100 portfolios as opposed to the default number which is 50
Spec = portfolioSpec()
setNFrontierPoints(Spec)<-100

#Convert the data to timeseries data
Portfolio <- as.timeSeries(Portfolio)

##Determine the efficient frontier and plot the same
effFrontier <- portfolioFrontier(Portfolio, Spec ,constraints = "LongOnly")
effFrontier

plot(effFrontier, c(1))


#From the two plots, we can see that the results are identical. 
#However, when working with portfolios with multiple assets, using the package will be more efficient and accurate

