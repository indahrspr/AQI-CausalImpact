# PACKAGES
library(forecast)
library(FitAR)
library(lmtest)
library(tseries)
library(psych)
library(ggplot2)

# DATA
data=read.csv(file.choose(),header=T, sep=",")
attach(data)
View(data)
dim(data)

# POLA DATA
data1=data[,2] #JKT
data2=inds <- seq(as.Date("2019-12-30"), as.Date("2020-12-02"), by = "day")
series=ts(data1, start=c(2019, as.numeric(format(data2, "%j"))),
          frequency=365) ; series
ts.plot(series,ylab="PM2.5",main="Jakarta Air Quality 2020-2021",col="red",lwd=2)
legend("bottomright",c("PM2.5"),cex=0.8,lty=5,text.font=2,col=c("red"))

#pearson correlation test
library(ggpubr)
x=data$JKT
y=data$TKY
cor.test(x,y,method="pearson")

#Causal Impact Analysis
library(CausalImpact)
data1
head(data1)
matplot(data1, type = "l",ylab="AQI Level",xlab="Time")

time.points <- seq.Date(as.Date("2019-12-30"), by = 1, length.out = 399)
data2=data[,2:3]
x=data2$JKT
y=data2$TKY
data <- zoo(cbind(x,y),time.points)
head(data)
nrow(data)
pre.period <- as.Date(c("2019-12-30", "2020-03-01"))
post.period <- as.Date(c("2020-03-02", "2020-12-02"))
impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
summary(impact)
