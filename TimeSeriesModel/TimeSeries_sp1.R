# Import thư viện và Load Data
library(ggplot2)
library(tseries)
library(forecast)
library(corrplot)

data<-read.csv("./DATA/AirPassengers.csv",stringsAsFactors = FALSE)
head(data)
class(data)

# Chuyển dataframe sang timeseries
dd<-ts(data[-1], frequency=12, start=c(1949,1), end = c(1960,12))
class(dd)
plot(dd)
abline(reg=lm(dd~time(dd)))
# Trực quan
# Kiểm tra tính cyclic
cycle(dd)
boxplot(dd~cycle(dd))
# Tính trend
trends<-decompose(dd, "multiplicative")
plot(trends)
# Chia dữ dữ liệu train và test
library(forecast)
# train <- head(dd, round(length(dd) * 0.8))
# h <- length(dd) - length(train)
# test <- tail(dd, h)
train_data_indices <- seq_len(length.out = floor(x = 0.8 * nrow(x = data)))
train_data <- data[train_data_indices, ]
test_data <- data[-train_data_indices, ]

# Phân rã dữ liệu
components<-decompose(dd, "multiplicative")
plot(components)
# trends<-decompose(dd, "multiplicative")
# plot(trends)

# Kiểm tra tính dừng
# dd2<-ts(data[-1], frequency=12, start=c(1949,1), end = c(1960,12))
# class(dd2)
adf.test(dd, alternative = "stationary")
dd2<-ts(train_data[-1], frequency=12, start=c(1949,1), end = c(1960,12))
class(dd2)
adf.test(dd, alternative = "stationary")

library("fUnitRoots")
urkpssTest(train_data, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(train_data, differences=1)
plot(tsstationary)

acf(train_data,lag.max=34)
# Kiểm định tương quan
M <- cor(dd)
corrplot(M, method = 'shade')
# Chuyển đổi chuỗi dừng
ts_diff1 <- diff(dd, lag = 1)  
tm <- cbind(dd, ts_diff1) 
head(tm) 
plot.ts(tm)
# Kiểm định lại tính dừng sau khi chuyển đổi
adf.test(dd, alternative = "stationary")
# ARIMA (p, d, q)
model<-auto.arima(dd)
model
# Predict
perdict<-forecast(model, level = c(95), h=10*12)
plot(perdict)