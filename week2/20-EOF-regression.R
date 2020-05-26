library(fma)

### Average as forecast

# Description: the beer data has montly Australian beer production 1/91 - 8/95

head(beer) #displays first few lines of the data

summary(beer) #descriptive statistics

plot(beer)

meanf(beer,1)
meanf(beer,5)

#Naive Method

naive(beer,1)
naive(beer, 5)

rwf(beer,5) #equivalent random walk

###Linear Regression

head(books) #displays first few lines of the data

summary(books) #descriptive statistics

meanf(books,1)


autoplot(books, ylab="Sales",xlab="Time", size=1)

fit <- lm(Paperback ~ Hardcover, data=books)  #Fit a linear regression model

summary(fit)

plot(Paperback ~ Hardcover, data=books, pch=19)
abline(fit)
