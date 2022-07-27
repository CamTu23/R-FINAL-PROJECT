# Import thư viện và Load Data
library(ggplot2)
library(tseries)
library(forecast)
library(corrplot)
# Đọc dữ liệu
data<-read.csv("./DATA/AirPassengers.csv",stringsAsFactors = FALSE)
head(data)
class(data)
# Chuyển dataframe sang timeseries
dd<-ts(data[-1], frequency=12, start=c(1949,1), end = c(1960,12))
class(dd)
#Trực quan
plot(dd)
abline(reg=lm(dd~time(dd)))
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
# adf.test(dd, alternative = "stationary")
dd2<-ts(train_data[-1], frequency=12, start=c(1949,1), end = c(1960,12))
class(dd2)
adf.test(dd2, alternative = "stationary")
# Kết quả:
#   Augmented Dickey-Fuller Test
# 
# data:  dd2
# Dickey-Fuller = -1.6418, Lag order = 5, p-value = 0.7256
# alternative hypothesis: stationary


library("fUnitRoots")
urkpssTest(dd2, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)
tsstationary = diff(dd2, differences=1)
plot(tsstationary)

acf(dd2,lag.max=115)
# Kiểm định tương quan
# M <- cor(dd)
# corrplot(M, method = 'shade')

# Chuyển đổi chuỗi dừng
ts_diff1 <- diff(dd2, lag = 1)  
tm <- cbind(tseries, ts_diff1) 
head(tm) 
plot.ts(tm) 
# ARIMA
model<-auto.arima(dd2)
model
# Kết quả:
# Series: dd2 
# ARIMA(2,1,2)(1,0,1)[12] with drift 
# 
# Coefficients:
#   ar1      ar2      ma1      ma2    sar1     sma1   drift
# 0.5193  -0.0862  -0.6087  -0.0702  0.8476  -0.6662  0.2669
# s.e.  0.5957   0.4105   0.5974   0.4870  0.1391   0.1982  3.0176
# 
# sigma^2 = 1396:  log likelihood = -718.3
# AIC=1452.61   AICc=1453.68   BIC=1476.31