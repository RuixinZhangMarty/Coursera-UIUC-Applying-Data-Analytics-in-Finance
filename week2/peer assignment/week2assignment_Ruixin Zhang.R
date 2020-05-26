
#############################################################
#
# Financial Analytics Assignment
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


data(housing)                   # This load the housing dataset into R


hstarts <- housing[,'hstarts']  # This line moves one column of housing 
# to a variable called hstarts.

# The data housing is a time series dataset with three columns.  
# We will only use one column to simplify the code.
# This data is the monthly housing starts from Jan. 1983 - Oct. 1989.

######################################################################
######################################################################
#
#*** START CODING:  Question 1

# Q1.A Inspect the first 10 elements of hstarts using the head() command.
head(hstarts, 10)


# Q1.B Plot the hstarts data using autoplot().
autoplot(hstarts)

# Q1.C Describe the plot in your own words.
# (answer here - remember to use hash tags for comments)
## There seems to have a seasonal trend in each year.
## Usually the plot reaches the maximum value of each year during middle of the year
## Then the plot decreses to its minimum value of each year in the end of the year
## There is a downward trend of the plot and the plot reaches its bottom at exactly 
## the first couple months of 1988

# Q1.D Plot the hstarts data using the seasonplot() command.
seasonplot(hstarts)


# Q1.E Describe and interpret theseason plot in your own words.
# (answer here - remember to use hash tags for comments)

## The y-axis denotes the hstarts while x-axis denotes the months.###
## There is a steep growth from February to April and a gradual decline from October to December###
## There can also be seen a YOY growth from 1983 to 1988.###
## There seems to exist a seasonal trend wherein, the prices skyrocket at the start of the year and tend to decline at the end of the year
## In 1983, prices steadily increased till June, then decreased in July, then again increased in August and then, finally going in a downward trajectory as per the seasonal trend
## In 1984, the prices again increased till June, then took a trip down south till October, wherein, they slightly increased to again decrease in the month of November and December
## In 1985, the prices increased in January to again decreased next month, then they increased again in the month of March to continue to it's trent till May, when it took a dive until, October, when there was a sharp increased only to have a decline in the last to months
## In 1986, just like in 1985 the price when up in January only to come down the next month, after that it skyrocketed till the month of May then went a downward trajectory for the rest of the year
## In 1987, the prices steadily increased till the month of June then it started declining
## The price at the start of 1988 was the lowest price ever seen at the start of the year, the prices increased from there but, only till April, after that, it started declining again
## In 1989, the prices increased till June then started declining for the rest of the year



#*** END CODING: Question 1



### QUESTION 2

# The next set of questions analyze the kkong dataset.  
# using linear regression.

data(kkong)   # This data set has height and weight of 21 gorillas.
attach(kkong) # This command allows you to reference just the column names.
# The column names are 'weight' and 'height'.



#*** START CODING:  Question 2


# Q2.A Use the summary() command on the column 'height' and 'weight'.
summary(height)
summary(weight)
summary(kkong)

# Q2.B Describe the results in your own words. Include

# a description of the range, min, max, and central tendencies will suffice.
### height ###
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. ###
### 13.00   25.00   29.00   35.14   35.00  150.00 ###

### weight ###
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. ###
### 31      41      47      50      53     130 ###



# Q2.C Plot the data using the command plot().
plot(kkong, type = 'l')

# Q2.D Describe the results in your own words.

#* The desciption should include a description of the graph and a description
#* of the data clustered around the bottom left corner 
#* as well as a discussion of the one outlier at the top left.

## Most of the data points are clustered together in the bottom left corner of the graph
## The weight of the graph is higher in the bottom left corner
## The distribution of point in that region is quite well
## The height is plotted on Y-axis and weight is plotted on X-axis ###
## Most of the observations are plotted between 0-60 units of weight and 0-40 units of height ###
## There is only one extreme observation which is >120 units of weight and >140 units of height ###
## The one extreme observation will be considered an outlier ###

# Q2.E Run a regression using height as the dependent variable
#      and store the results in 'fit'.  Use the summary() command to see 
#      the results.

fit = lm(height ~ weight, data = kkong)
summary(fit)


# Q2.F From the results answer the following (short answer):
#   - What is the coefficient for weight?
#   - Is the coefficient significant?
#   - What does the coefficient mean? In other words, interpret the coefficient.
#   - What is the Adjusted R-squared and what does it mean?

## 1.3078
## Yes
## this implies 1 unit change in weight will lead to 1.3078 units change in height
## Adjusted R-squared is 0.8782 and it means that the linear regression fits the data very well



#*** END CODING:  Question 2


#*** START CODING:  Question 3

# In this question, we will using the hstarts time series used in Question 1.

# Q3.A  Create a moving average of hstarts using 5 lags and store it in hstartsMA5.
hstartsMA5 = ma(hstarts,5)


# Q3.B  Create a moving average of hstarts using 15 lags and store it in hstartsMA15.
hstartsMA15 = ma(hstarts,15)

# RUN the following lines to make a graph.
plot(hstartsMA5, col = 'red')
lines(hstartsMA15, col = 'green')


# Q3.C  Describe the two moving average plots in your own words.
## The curve with 15 lags is way smoother than the one with 5 lags.
## The curve with 5 lags is also very close to the original curve.


#*** END CODING:  Question 3


#*** START CODING:  Question 4
# 

# Q4.A Create a simple exponential smoothing series using ses()
#     with h=5 lags and store it in hstartsSES5
hstartsSES5 = ses(hstarts, h = 5)


# Q4.B What is the RMSE?  (Use the accurancy() command.)
accuracy(hstartsSES5)
## RMSE = 19.46083

# Q4.C Create a Holt-Winters model using the hw() command and store it in hstartsHWa 
#      using the option : seasonal = "additive". 
hstartsHWa = hw(hstarts, seasonal = "additive")


# Q4.D Create a Holt-Winters model using the hw() command and store it in hstartsHWm 
#      using the option : seasonal = "multiplicative".
hstartsHWm = hw(hstarts, seasonal = "multiplicative")


# run the following to create the plot

autoplot(hstarts) +
  autolayer(hstartsHWa, series="HW additive forecasts",
            PI=FALSE) +
  autolayer(hstartsHWm, series="HW multiplicative forecasts",
            PI=FALSE) +
  ggtitle("Housing Starts") +
  guides(colour=guide_legend(title="Forecast"))

# Q4.E Which model (additive or multiplicative) looks better and why?

# A good answer will describe the two forecast graphs and also describe how the the multiplicative
# forecast will use a percentage of past changes.

#* Sample answer
#* Both models are good possibilities for forecasts as they both exibit the cyclical cycles as well 
#* the downward trend from 1987 onward.  However, the multiplicative mode also seems to dampen the cycles.
#* A visual inspection of the previous 3-4 cycles seem to confirm the dampening of the cycles.

## The multiplicative model is better as it is smoother and have lesser RMSE although by only 0.032446 units
## Also, multiplicative method is preferred when the seasonal variations are changing proportional to the level of the series
## Both models are good possibilities for forecasts as they both exibit the cyclical cycles as well
## the downward trend from 1987 onward.  However, the multiplicative mode also seems to dampen the cycles.
## A visual inspection of the previous 3-4 cycles seem to confirm the dampening of the cycles.  
## Both forecasts wold suggest the possibility to reach new minimum values and this
## possibility should be somehow evaluated with a judjmental approach in order to evaluate
## also the general macroeconomic framework.

#*** END CODING:  Question 4





