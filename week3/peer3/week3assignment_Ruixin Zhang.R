
#############################################################
#
# Financial Analytics Assignment 2
#
# In this assignment, you are asked to use some of the 
# commands demonstrated in the video examples to analyze
# the problem.  You will then be asked to interpret the 
# results.  
#
# For this assignment, you may use any resources necessary
# to be able to exectute the code.  However, the interpretation 
# should reflect your thinking.
#
# You are to work on the problem alone, without help 
# from any other person.
#
###############################################################

# INSTRUCTIONS:  Enter your code between the comments line 
# as directed.  Comments begin with a hashtag '#'.
# For example

### Start Code

### End Code

# If you are asked to interpret results, make sure to put your
# answers in a comment line.

################################################################ 

###  required libraries  ###
# Run the following code to load the packages (libraries)
# If the library package does not load properly, make sure 
# that you have installed the package onto  your system.  
# Use the 'Packages' tab in RStudio to install the package.

library(fma)
library(ggplot2)




### end required libraries

### Run the following code


data(pigs)                   # This load the  dataset into R from fma library

# see https://cran.r-project.org/web/packages/fma/fma.pdf for details.


# The data a time series of monthly total number of pigs
# slaughtered in Victoria, Australia (Jan 1980 - Aug 1995)
# 

#*** START CODING: Question 1

# Q.1  Display the first 24 data points using the head() function.  Familiarize
# yourself with the structure of the data.

head(pigs,24)

# Q.2 Use the autoplot()  command to graph the pigs data.

autoplot(pigs)

# Q.3 Describe the plot.

## The x-axis is time in months.  The y-axis is the number of  pigs slaughtered in Vitoria from Jan 1980 - Aug 1995. 
## There seems to be an upward trend from around 1983 to 1985, 
## a downward rrend from 1985 to 1987, 
## and then an upward trend from 1987 thru the end of the data in 1995.

# Q.4 Use the seasonplot()  command to graph the pigs data.

seasonplot(pigs)

# Q.5 Describe the plot.

## The y-axis is the number of pigs slaughtered and the x-axis is the month.
## Each line represents a year of data. for each year, there dos not seem to be any discernible seasonal patterns.
## There was a sudden drop of pigs slaughtered in March.
## Upon inspection of the data and using the min() command, the sudden drop happened in March of 1980.


# Q.6 Create an ARIMA model using the auto.arima() command and the pigs data. 
auto.arima(pigs)

# Q.7 What are the paremeters for the ARIMA model using the data 'pigs'? 
# In other words what is the model (ARIMA (p,d,q)(P,D,Q)) and what do the parameters mean?

# The model was ARIMA(2,1,0)(2,0,0).  The best model used two lags for the autoregressive component,
# one level of differencing, and two seasonal autoregressive terms.

#Q.8 What is the AIC value?

# Answer: 3954.06

#Q.9 What is the AIC used for?

# The AIC is used to compare the goodness-of-fit among competing models.





