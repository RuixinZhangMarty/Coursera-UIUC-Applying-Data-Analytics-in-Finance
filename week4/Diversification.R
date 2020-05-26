```{r}
library(fPortfolio)
library(quantmod)
library(ggplot2)
library(BatchGetSymbols)
library(timeSeries)

#Obtaining Stock Tickers and Calculating Stock Returns Data
# First, we need to get the tickers of all the stocks in the SnP500 Index
sp500 <- GetSP500Stocks()
sp500tickers <- sp500[,c(1,2,4)]


#Select n stocks from S&P 500
n = 5
randomstocks1 <- c(sample.int(500, n))
stocks1 <- sp500tickers[randomstocks1,1]
prices1 <- getSymbols(stocks1[1], source="yahoo", auto.assign=FALSE,
                     return.class="xts")[,6]

for (i in 2:n){
  prices.tmp <- getSymbols(stocks1[i], source="yahoo", auto.assign=FALSE,
                     return.class="xts")[,6]
  prices1 <- cbind(prices1, prices.tmp)
}
colnames(prices1) <- sp500tickers[randomstocks1,2]

m = 10
randomstocks2 <- c(sample.int(500, m))
stocks2 <- sp500tickers[randomstocks2,1]
prices2 <- getSymbols(stocks2[1], source="yahoo", auto.assign=FALSE,
                     return.class="xts")[,6]

for (j in 2:m){
  prices.tmp <- getSymbols(stocks2[j], source="yahoo", auto.assign=FALSE,
                     return.class="xts")[,6]
  prices2 <- cbind(prices2, prices.tmp)
}
colnames(prices2) <- sp500tickers[randomstocks2,2]


Portfolio1 <- na.omit(diff(log(prices1)))
Portfolio2 <- na.omit(diff(log(prices2)))

Portfolio2 <- Portfolio2["2015/"]
Portfolio1 <- Portfolio1["2015/"]

Portfolio2 <- as.timeSeries(Portfolio2)
Portfolio1 <- as.timeSeries(Portfolio1)


#Portfolio 1 - Tech Stocks

##Determine the efficient frontier and plot the same
effFrontier1 <- portfolioFrontier(Portfolio1,constraints = "LongOnly")
effFrontier1

plot(effFrontier1, c(1,2,3,4))


##Plot the weights for all the portfolio in the efficient frontier
frontierWeights1 <- getWeights(effFrontier1)
barplot(t(frontierWeights1), main="Frontier Weights", col=cm.colors(ncol(frontierWeights1)+2), legend=colnames(frontierWeights1))



##Obtain the weights for each stock for the portfolio with the least variance
mvp1 <- minvariancePortfolio(Portfolio1, spec=portfolioSpec(),constraints="LongOnly")
mvp1
tanPort1 <- tangencyPortfolio(Portfolio1, spec=portfolioSpec(), constraints="LongOnly")
tanPort1
minvarweights1 <- getWeights(mvp1) 
tanportweights1 <- getWeights(tanPort1)
mvpret1 <- ((getTargetReturn(mvp1)[1])+1)^252 - 1
tanret1 <- ((getTargetReturn(tanPort1)[1])+1)^252 - 1
mvprisk1 <- getTargetRisk(mvp1)[4] * sqrt(252)
tanrisk1 <- getTargetRisk(tanPort1)[4] * sqrt(252)
return1 <- cbind(tanret1, mvpret1)
risk1 <- cbind(tanrisk1, mvprisk1)
parameters1 <- rbind(return1,risk1)
colnames(parameters1) <- c("Tangency Portfolio","Min Var Portfolio")
parameters1
wt1 <- (cbind(minvarweights1, tanportweights1))
colnames(wt1) <- c("Minimum Variance Portfolio", "Tangency Portfolio")


##Plot the weights of the minimum variance portfolio
weights1 <- data.frame(minvarweights1)
assets <- colnames(frontierWeights1)
ggplot(data=weights1, aes(x=assets, y=minvarweights1, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",minvarweights1*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
              ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
                labs(x= "Assets", y = "Weight (%)")


##Plot the weights of the tangency portfolio
tanwt1 <- data.frame(tanportweights1)
assets <- colnames(frontierWeights1)
ggplot(data=tanwt1, aes(x=assets, y=tanportweights1, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tanportweights1*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
              ggtitle("Tangency Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
                labs(x= "Assets", y = "Weight (%)")


##Tabulate the risk and return for each point on the efficient frontier

riskReturnPoints1 <- frontierPoints(effFrontier1) # get risk and return values for points on the efficient frontier
annualizedPoints1 <- data.frame(targetRisk=riskReturnPoints1[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints1[,"targetReturn"] * 252)

annualizedPoints1 <- cbind(frontierWeights1, annualizedPoints1)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0
plot((annualizedPoints1[,"targetReturn"]-riskFreeRate) / annualizedPoints1[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio", col = "blue", pch = 20, main="Sharpe Ratio Curve")

#Portfolio 2 - Multi-Sector Stocks

##Determine the efficient frontier and plot the same
effFrontier2 <- portfolioFrontier(Portfolio2, constraints = "LongOnly")
effFrontier2
plot(effFrontier2, c(1,2,3,4))

##Plot the weights for all the portfolio in the efficient frontier
frontierWeights2 <- getWeights(effFrontier2)
barplot(t(frontierWeights2), main="Frontier Weights", col=cm.colors(ncol(frontierWeights2)+2), legend=colnames(frontierWeights2))


##Obtain the weights for each stock for the portfolio with the least variance
mvp2 <- minvariancePortfolio(Portfolio2, spec=portfolioSpec(),constraints="LongOnly")
mvp2
tanPort2 <- tangencyPortfolio(Portfolio2, spec=portfolioSpec(), constraints="LongOnly")
tanPort2
minvarweights2 <- getWeights(mvp2) 
tanportweights2 <- getWeights(tanPort2)
mvpret2 <- ((getTargetReturn(mvp2)[1])+1)^252 - 1
tanret2 <- ((getTargetReturn(tanPort2)[1])+1)^252 - 1
mvprisk2 <- getTargetRisk(mvp2)[4] * sqrt(252)
tanrisk2 <- getTargetRisk(tanPort2)[4] * sqrt(252)
return2 <- cbind(tanret2, mvpret2)
risk2 <- cbind(tanrisk2, mvprisk2)
parameters2 <- rbind(return2,risk2)
colnames(parameters2) <- c("Tangency Portfolio","Min Var Portfolio")
parameters2
wt2 <- t(cbind(minvarweights2, tanportweights2))
row.names(wt2) <- c("Minimum Variance Portfolio", "Tangency Portfolio")



##Plot the weights of the minimum variance portfolio
weights2 <- data.frame(minvarweights2)
assets <- colnames(frontierWeights2)
ggplot(data=weights2, aes(x=assets, y=minvarweights2, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",minvarweights2*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
              ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
                labs(x= "Assets", y = "Weight (%)")

##Plot the weights of the tangency portfolio
tanwt2 <- data.frame(tanportweights2)
assets <- colnames(frontierWeights2)
ggplot(data=tanwt2, aes(x=assets, y=tanportweights2, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tanportweights2*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
              ggtitle("Tangency Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
                labs(x= "Assets", y = "Weight (%)")


##Tabulate the risk and return for each point on the efficient frontier

riskReturnPoints2 <- frontierPoints(effFrontier2) # get risk and return values for points on the efficient frontier
annualizedPoints2 <- data.frame(targetRisk=riskReturnPoints2[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints2[,"targetReturn"] * 252)

annualizedPoints2 <- cbind(frontierWeights2, annualizedPoints2)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0
plot((annualizedPoints2[,"targetReturn"]-riskFreeRate) / annualizedPoints2[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio", col = "blue", pch = 20, main="Sharpe Ratio Curve")


```

