####
# PROJECT: ANALYSIS OF S&P 500 STOCKS
# 
# TEAM: Ishan Dabholkar (IDabholkar@my.harrisburgu.edu)
#       Justin Firmino (JFirmino@my.harrisburgu.edu)
#       Bharadwajkachpuram (BKachapuram@my.harrisburgu.edu)
# 
# SUBJECT: ANLY 502-Prof. Bradley Faith
#
####

## Install needed packages

#install.packages("quantmod")
#install.packages("plotly")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("usmap")
#install.packages("ggmap")
#install.packages("dplyr")
#install.packages("neuralnet")


### Include the needed libraries

library(readxl)
library(ggplot2)
library(plotly)
library(usmap)
library(ggmap)
library(dplyr)
library(neuralnet)
library(psych)
library(pastecs)
library(Hmisc)
library(BSDA)
library(magrittr)


## Read in input files using read_excel function

fundamentals <- read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/fundamentals.xls")
price = read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/prices.xls")
split = read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/prices-split-adjusted.xls")
security = read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/security.xls")

### Sort the exel files depending on stock symbols
price = price[order(price$symbol),]
split = split[order(price$symbol),]
fundamentals = fundamentals[order(fundamentals$`Ticker Symbol`),]
security = security[order(security$`Ticker symbol`),]

W = split[1:251,]

#### Candlestick pattern

#### Part 1- Table can have multiple rows of same symbol
### uniquify those symbols to consider those symbols
for(i in unique(price$symbol))
{
  ### Form a new data frame for each symbol stock
  sub = subset(price, price$symbol == i)
  
  ### Plot closing value
  plot(sub$close , type ="o", main = cbind("Close of ", i) , col = "red")
  par(new= TRUE)
  
  #### Plot opening values
  plot(sub$open , type ="o", main = cbind("Open of ",i),  col = "blue")
  par(new= TRUE)
  
  
  ### Plot low values of the stock
  plot(sub$low , type ="o", main = cbind("Low of ", i) , col ="orange")
  par(new= TRUE)
  
  ### Plot high values of the stock
  plot(sub$high , type ="o", main = cbind("High of ", i))
  
  #### Import plot_ly function from plotly library to plot the candlestick pattern
  #### This shows all open, low, close, high on a single plot with green and red colors
  
  print(plot_ly(x = sub$date, type = "candlestick" , open = ~sub$open, close=  ~sub$close , high = ~sub$high,  low =  ~sub$low))
}

#### Distribution on USA
#states = unique(security$`Address of Headquarters`)
states = security$`Address of Headquarters`


### Use google geocode to import the latitude and longitude of the company adress
## This queries google server for each location
## WARNING: it can take some time
code = geocode(states)

### COnvert the lat and long values in a data frame
df = as.data.frame(code)

## remove na values if any
df = na.omit(df)

### See the world distribution of company headquater location

qmplot(lon, lat, data = df, color =I('red'), size = I(1.3), darken = 0.3)

#### Get the US map--- 
### Ignore stocks outside the US for better viewing
## Arrange the map from the center point of US

ggmap(get_map(zoom = 4, crop = TRUE, scale = 2)) + geom_density2d(data = df, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = df, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

### Cmparing the bay area geographical wise
ggmap(get_map(zoom = 9, crop = TRUE, scale = 2, location = "San Francisco")) + geom_density2d(data = df, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = df, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
##### Distribution of companies based on sector

## Plot the different types of sector of stocks in S&P 500
gisSector = table(security$`GICS Sector`)

barplot(gisSector, xlab = "Category for stocks", ylab = "Number of stocks in a category", col = "blue")

## Plot pie chart to show sector wise distribution

pie(table(security$`GICS Sector`), main = "Sector wise distribution")

## Plot pie chart for each type of sub category inside a category
# Uniquify the sector and process
for(i in unique(security$`GICS Sector`)) {
  
  subCategory = subset(security, security$`GICS Sector` == i)
  
  pie(table(subCategory$`GICS Sub Industry`), main = paste("Sub Sector distribution for ",i))
  
}

######### Analysis

## SPlit the input data 70 % and 30 % for training and testing
train = sample_frac(split, 0.7)

sid = as.numeric(rownames(train)) # because rownames() returns character

test = split[-sid,]

### Arrange the training and testing data frame in order of stock symbols ascending order
train = train[order(train$symbol),]
test = test[order(test$symbol),]

###### Prediction using Linear model

linear_model_train = lm(open~., data = train)

## Do predictive analysis using this linear model
predict_linear_model_test = predict(linear_model_train, test , se.fit = TRUE)

## Import caret library to compare the result of predictive value vs expected values
library(caret)

compareResult_linear_model = postResample(predict_linear_model_train$fit, test$open)

### Predict the future values using neural models

## Import the nnet library for Neural net modelling
library(nnet)

nnet_model_train = nnet( open~. , train)

## Predict the values using the nnet model on test data (30 % data)
predict_test = predict(nnet_model_train, test)


## Create a new data frame with predicted values on the test data frame
predict_model_nnet_test = data.frame(test, as.data.frame(predict_test))


### Rename the colunms of the data frame
colnames(predict_model_nnet_test) = c("date","symbol","open","close","low","high","volume","predicted_open")

## Same logic as used for plotting candlestick pattersn:
## Uniquify the symbols from the training data set 

for(i in unique(train$symbol))
{

  ## Subset the data set for each type of symbol
  sub = subset(predict_model_nnet_test, predict_model_nnet_test$symbol == i)
  
  
  ## Plot the actual values from test data set
  print(plot(sub$open , type ="o", main = cbind("Close of ", i) , col = "red"))
  par(new= TRUE)
  
  ## Plot the new (predicted) values from the nnet model, which was appending in the data frame above
  print(plot(sub$predicted_open , type ="o", main = cbind("Predicted Close of ", i) , col = "blue"))
  
}

## Use PostResample function, from library caret to caompare the predicted and expected values

compareResult = postResample(sub$open, sub$predicted_open)
