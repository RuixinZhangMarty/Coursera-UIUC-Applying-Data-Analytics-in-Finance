#Setting up the required libraries
library(quantmod)
library(tidyverse)
library(TTR)

#Obtaining stock price data for Advanced Auto Parts Inc
AAP <- getSymbols("AAP", source="yahoo", auto.assign=FALSE,
                    return.class="xts")[,6]

#Calculate returns
AAPret <- diff(log(AAP))
colnames(AAPret)  <- "AAP"

#Trim the dataset 
AAPret <- AAPret["2010/"]
AAP <- AAP["2010/"]

plot(AAP)

#Generate Simple Moving Averages
sma26 <- SMA(AAP, 26)
sma12 <- SMA(AAP, 12)

Data <- na.omit(as.data.frame(cbind(AAP, AAPret, sma12, sma26)))
colnames(Data) <- c("AAPPrices","AAPRet","SMA12","SMA26" )


#Condition for trend following strategy

Data$UD <- ifelse(Data$SMA12 >= Data$SMA26, 1, 0)
class(Data$UD)

#Devise a trading strategy and Backtest

Data$Trade <- ifelse(Data$UD == 1, "BUY", "SELL")
Data$Position <- ifelse(Data$Trade == "BUY", 1, -1)
Data$AlgoRet <- Data$AAPRet * Data$Position
AnnualizedReturn <- ((mean(Data$AlgoRet)+1)^252 - 1)
plot(AAPret)
Standev <- sd(Data$AlgoRet)
rf <- 0.02
SharpeRatio <- (AnnualizedReturn - rf)/Standev

#Print the results
print(paste("The trend-following algorithm was applied to the AAP Stock prices and was able to achieve an Annualized Return of", AnnualizedReturn,"%"))



#ppt
plot(Data$AAPPrices, type = "l", col = "red", xlab = "Prices")
par(new = TRUE)
plot(Data$SMA12, type = "l" , col = "green")
par(new = TRUE)
plot(Data$SMA26, type = "l" , col = "blue")

