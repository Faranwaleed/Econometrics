# Logistic Regression 

# Set working directory 
setwd("C:/Users/Lenovo/Desktop/R")

# Read in the data 
install.packages("readr")
library(readr)

td <- read.csv("Turnover.csv")
View(td)

# ID represents Employee ID 
# Turnover (Binary Variable) our outcome variable 1 indicates that employee quit
# 0 indicates that employee is still with company 
# JS indicates Job Satisfaction 
# OC indicates Organizational Commitment 
# TI indicates turnover intentions
# NAff indicates Negative Affectivity 


# Does Job satisfaction predicts employee turnover behavior (Simple Regression Analysis)

# Statistical Assumptions 
# 1) cases are randomly sampled 
# 2) data are free of bivariate/multivariate outliers 
# 3) outcome variable is dichotomous
# 4) association between continuous predictor and the logit transformation
#    of the outcome variable is linear 
# 5) free of collinearity (in case of multiple regression analysis)

# Simple logistic regression model 
install.packages("lessR")
library(lessR)
Logit(Turnover ~ JS, data = td)

# Check the assumptions 
# random sample : Assume that it is met 
# outcome variable is dichotomous : met 
# free of bivariate outliers (analysis on cooks value above 0.4 or any value that stand out from the others) : met
# assume that person in row number 69 and 7 are potential outliers 

# if fitted value is less than 0.5 they are predicted to be with company
# if fitted value is above 0.5 that are predicted to quit the company 
# association between continuous predictor and the logit transformation
#     of the outcome variable is linear : met

# BoxTidwell Test 

Logit(Turnover ~ JS + JS:log(JS), data = td, brief=T)

# if the interaction term between JS and log(JS) is statistically significant 
# there might be a non-linear relationship between the variables  
# p-value is greater than 0.05 (0.232) which suggest that there is no non-linear relationship 

Logit(Turnover ~ JS, data = td)

# Interpretation : JS does have a statistically significant association 
# Every one unit increase in JS the logistic function decreases by 0.4378
# However, this is not so informative 
# We do know that the association is significant and negative 

# We need to interpret the odd ratio, and we know that if odd ratio is less than 1 
# it represent the negative relationship and if it greater than 1 it represent 
# positive relationship 

# Interpretation: 1 - 0.646 = 0.354 (35.4%) the odds of quiting are reduced by 
# 35.4% if JS increase by 1 unit

# Overall model fit: 
# Predicted accuracy is about 58.2% 

# Simple regression model without outliers 

Logit(Turnover ~ JS, data = td, rows = (!ID %in% c("EMP873", "EMP607")))

# Multiple Logistic Regression Model 

Logit(Turnover ~ JS + TI + NAff, data = td)

# Check the assumptions 

# random sample : Assume that it is met 

# outcome variable is dichotomous : met 

# free of multivariate outliers: Check Cooks distance value (assume: met) 

# free of collinearity (Predicted variables are not strongly correlated to each other): met

# to check collinearity look at the values of tolerance and VIP (Variance inflation factor)
# tolerance is just the reciprocal of VIP, we will focus on tolerance 
# if tolerance is equal to 1 no collinearity and 0 perfect collinearity 

# association between continuous predictor and the logit transformation
#     of the outcome variable is linear : met

Logit(Turnover ~ JS + JS:log(JS) + TI + TI:log(TI) + NAff + NAff:log(NAff), data = td)

# if the interaction term is statistically significant 
# there might be a non-linear relationship between the variables
# Here no evidence of non-linear relationship between variables found 

Logit(Turnover ~ JS + TI + NAff, data = td)

# Interpretations: Odds ratio: The odds of quiting increase by 2.45 times when TI increase by 1 unit 
# the odds of quiting increase by 3.3 times when NAff increase by 1 unit 

# Overall model fit: 
# Predicted accuracy is about 78.9% 