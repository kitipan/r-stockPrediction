library(forecast)
library(astsa)
library(readxl)
library(e1071)
library(prophet)

RawPrice_Data_Jan2017 <- read_excel("~/GitHub/r-stockPrediction/input/RawPrice_Data_Jan2017.xlsx", 
                                    skip = 7)
stock <- RawPrice_Data_Jan2017
names(stock) <-  c("date","set","cpall","ptt","pttep","banpu","kbank","cpf","lh","scC","advance")
rm(RawPrice_Data_Jan2017)

s1 <-impute(stock[,2:10], what='mean')
ts_s1 <- ts(s1[,(-1)], start=c(2007,15), frequency = 365)

cpall1 <- ts_s1[,1]
cpall <- window(cpall1, start = 2010)
d_cpall <- diff(cpall)
acf2(cpall, max.lag = 30)
plot(cpall1)

#using prophet for CPall, rename column names
procpall <- stock[,c(1,3)]
names(procpall) <- c("ds","y")
m <- prophet(procpall)

#crete df slot of 1 year and plot
n <- make_future_dataframe(m, periods = 365)
o <- predict(m,n)
plot(m,o)


