#Installing libraries essential for portfolio construction
library(fPortfolio)
library(quantmod)
library(ggplot2)
library(BatchGetSymbols)
library(timeSeries)

#Obtaining Stock Tickers and Calculating Stock Returns Data
# First, we need to get the tickers of all the stocks in the SnP500 Index
sp500 <- GetSP500Stocks()
sp500tickers <- sp500[,c(1,2,4)]


#We select 5 Leading Tech Hardware Stocks and download the price data for those

stocks1 <- c("AAP", "INTC", "CSCO", "NVDA", "TXN")
prices1 <- getSymbols(stocks1[1], source="yahoo", auto.assign=FALSE,
                      return.class="xts")[,6]
for (i in 2:length(stocks1)){
  prices.tmp <- getSymbols(stocks1[i], source="yahoo", auto.assign=FALSE,
                           return.class="xts")[,6]
  prices1 <- cbind(prices1, prices.tmp)
}
colnames(prices1) <- c("Apple", "Intel", "Cisco", "Nvidia", "Texas Instruments")

plot(prices1$Apple)
plot(prices1$Intel)
plot(prices1$Nvidia)
plot(prices1$Cisco)
plot(prices1$`Texas Instruments`)


#Since we will be working with returns, let us convert the price data to returns 

Portfolio1 <- na.omit(diff(log(prices1)))

#Trimming the Data to get recent data post 12-31-2014
Portfolio1 <- Portfolio1["2015/"]

mean(Portfolio1$Apple)
var(Portfolio1$Apple)
mean(Portfolio1$Intel)|
var(Portfolio1$Intel)
mean(Portfolio1$Cisco)
var(Portfolio1$Cisco)
mean(Portfolio1$Nvidia)
var(Portfolio1$Nvidia)
mean(Portfolio1$`Texas Instruments`)
var(Portfolio1$`Texas Instruments`)

means <- c(mean(Portfolio1$Apple),mean(Portfolio1$Intel),mean(Portfolio1$Cisco),mean(Portfolio1$Nvidia),mean(Portfolio1$`Texas Instruments`))
vars <- c(var(Portfolio1$Apple),var(Portfolio1$Intel),var(Portfolio1$Cisco),var(Portfolio1$Nvidia),var(Portfolio1$`Texas Instruments`))

Stockplot <- as.data.frame(t(cbind(vars, means)))
colnames(Stockplot)<- c("Apple", "Intel", "Cisco", "Nvidia", "Texas Instruments")
Stockplot <- t(Stockplot)

plot(Stockplot, col = rainbow(5), pch= 15, xlab = "Variance", ylab = "Mean Returns", main = "Risk vs Return")
legend("bottomright", legend=c("Apple", "Intel", "Cisco", "Nvidia", "Texas Instruments"),
       col=rainbow(5), lty=1:2, cex=0.8, pch = 15)

#Convert the numeric vectors to timeseries vectors

Portfolio1 <- as.timeSeries(Portfolio1)

#Let us build portfolio using each of the stock combinations to obtain the minimum variance portfolio in each of the cases


#Set Specs

Spec = portfolioSpec()
setNFrontierPoints(Spec)<-100
#Portfolio 1 - Tech Stocks

##Determine the efficient frontier and plot the same
effFrontier1 <- portfolioFrontier(Portfolio1, Spec ,constraints = "LongOnly")
effFrontier1

plot(effFrontier1, c(1))


##Plot the weights for all the portfolio in the efficient frontier
frontierWeights1 <- getWeights(effFrontier1)
barplot(t(frontierWeights1), main="Frontier Weights", col=cm.colors(ncol(frontierWeights1)+2), legend=colnames(frontierWeights1))



##Obtain the weights for each stock for the portfolio with the least variance
mvp1 <- minvariancePortfolio(Portfolio1, spec=portfolioSpec(),constraints="LongOnly")
mvp1

##Obtain the weights for each stock for the tangency portfolio
tanPort1 <- tangencyPortfolio(Portfolio1, spec=portfolioSpec(), constraints="LongOnly")
tanPort1

#Let us tabulate the weights for the two portfolios for comparison
minvarweights1 <- getWeights(mvp1) 
tanportweights1 <- getWeights(tanPort1)
weights1 <- (cbind(minvarweights1, tanportweights1))
colnames(weights1) <- c("Minimum Variance Portfolio", "Tangency Portfolio")

