#install.packages("quantmod")
#install.packages("plotly")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("usmap")
#install.packages("ggmap")


library(readxl)
library(ggplot2)
library(quantmod)
library(plotly)
library(usmap)
library(ggmap)
fundamentals <- read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/fundamentals.xls")
price = read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/prices.xls")
split = read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/prices-split-adjusted.xls")
security = read_excel("Desktop/Harrisburg/ANLY502- Analy methods1/security.xls")

price = price[order(price$symbol),]
split = split[order(price$symbol),]
fundamentals = fundamentals[order(fundamentals$`Ticker Symbol`),]
security = security[order(security$`Ticker symbol`),]

W = split[1:251,]
#### Candlestick pattern
for(i in unique(price$symbol))
{
  sub = subset(price, price$symbol == i)
  
  plot(sub$close , type ="o", main = cbind("Close of ", i) , col = "red")
  par(new= TRUE)
  
  plot(sub$open , type ="o", main = cbind("Open of ",i),  col = "blue")
  par(new= TRUE)
  
  plot(sub$low , type ="o", main = cbind("Low of ", i) , col ="orange")
  par(new= TRUE)
  
  plot(sub$high , type ="o", main = cbind("High of ", i))
  
  
  sub = subset(price, price$symbol == i)
  
  print(plot_ly(x = sub$date, type = "candlestick" , open = ~sub$open, close=  ~sub$close , high = ~sub$high,  low =  ~sub$low))
}

#### Distribution on USA
#states = unique(security$`Address of Headquarters`)
states = security$`Address of Headquarters`

code = geocode(states)
df = as.data.frame(code)

map = get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 4,
                      maptype = "satellite", scale = 2)

ggmap(map) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

usa_center = as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")


##### Distribution of companies based on sector
gisSector = table(security$`GICS Sector`)

barplot(gisSector, xlab = "Category for stocks", ylab = "Number of stocks in a category", col = "blue")




