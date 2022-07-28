#Import library
library(forecast)
library(tseries)
library(dplyr) #data manipulation
library(ggplot2) #data visualization
library(DT) #beautify the data table
library(plotly) #make a pie chart
library(Metrics) #find error value
library(Hmisc)
library(cowplot)
library(WVPlots)
library(pearson7)
library(caTools)
library(astsa)
library(fpp2)
library(rpart)

#Load data
f <- file.choose() # chọn file AirPassengers.csv trong thư mục DATA của TimeSeriesModel
data_airpassenger <- read.csv(f)

#Understading data
describe(data_airpassenger)
# Không có giá trị null, 2 biến, 144 quan sát

#Preprocessing

# turning into the time series data frame

airpassengers <- ts(data_airpassenger[-1], start = c(1949,1), end = c(1960,12), frequency =12) # frequency = 12 do data có cột month là dữ liệu hầng tháng qua các năm
plot(airpassengers, col = "Blue", ylab = "Number of Passengers", height = 100, main = "Biểu diễn số lượng hành khách qua các năm")
log_air_passengers <- log(airpassengers)
plot(log_air_passengers, ylab="log(airpassengers)", col='red')

# Split data to train and test

train_air_indices <- seq_len(length.out = floor(x = 0.8 * nrow(x = data_airpassenger)))
train_air <- data_airpassenger[train_air_indices, ]
test_air <- data_airpassenger[-train_air_indices, ]

#Phân rã
train_airpassengers <- ts(train_air[-1], start = c(1949,1), end = c(1958,7), frequency =12) # frequency = 12 do data có cột month là dữ liệu hầng tháng qua các năm
# log_train_airpassenger <- log(train_airpassengers)
test_airpassengers <- ts(test_air[-1], start = c(1958,8), end = c(1960,12), frequency =12)
log_train_airpassenger <- log(train_airpassengers)
log_train_airpassenger_diff<- diff(log_train_airpassenger, lag = 1)  
decompose_train_data <- stl(log_train_airpassenger[,1], s.window = "period") #hàm stl phân rã chuỗi thời gian, phân tích theo mùa, xu hướng bằng cách sử dụng LOESS (Local Regression)
plot(decompose_train_data, main="Phân rã chuỗi thời gian theo mùa, theo xu hướng", col = "blue")

head(fit$time.series) # các đối tượng của các biến quan sát
head(exp(fit$time.series)) # chuyển đổi sự phân rã trở lại số liệu ban đầu
# Hình dung sự phân rã theo tháng theo năm
monthplot(train_airpassengers, xlab="Month", ylab="Passenger", main="Số lượng hành khách theo tháng", col = "blue") #Cho thấy số lượng hành khách biến động theo từng tháng và số lượng trung bình hành khách theo từng tháng, cao nhất ở tháng 6,7,8. Nhìn chung số lượng hành khách mỗi tháng tăng dần đều.
seasonplot(train_airpassengers, year.labels="TRUE", main="Số lượng hành khách theo năm", col = 1:11) # tăng dần theo năm

# Kiểm tra dữ liệu có dừng không?
adf.test(train_airpassengers) # 0.01 smaller -> non-stationary  

#KIỂM ĐỊNH TỰ TƯƠNG QUAN
acf(train_airpassengers) # q
pacf(train_airpassengers) # p
# => Không phải chuỗi nhiễu trắng

#CHUYỂN DỮ LIỆU VỀ DỪNG
# sai phân bậc 1
train_airpassengers_diff <- diff(train_airpassengers)
plot(train_airpassengers_diff)
autoplot(train_airpassengers_diff) + ggtitle("Bieu dien sai phan bac 1") + ylab("Number of passengers")

# acf2(diff(train_airpassengers))
# check_stationary <- cbind(train_airpassengers, train_airpassengers_diff)
# plot(check_stationary)

#Kiểm tra lại

adf.test(train_airpassengers_diff)
acf(train_airpassengers_diff) # q
pacf(train_airpassengers_diff) # p
#Create model
# arima_air = auto.arima(train_airpassengers, ic = "aic", trace = TRUE)
#   arima lấy mô hình nào có 3 giá trị p,d,q có aic nhỏ nhất thì lấy (2,1,1)
# ar lấy mô hình nào d=0, q = 0 (1,0,0)
# arma lấy mô hình nào d = 0, (2,0,1)

arima_air = auto.arima(train_airpassengers,ic = "aic", trace = TRUE) # chạy arima
arima_air = auto.arima(train_airpassengers,ic = "aic", trace = TRUE, d =0) # chạy ar, arma

# Chạy mô hình arima
arima_model <- arima(train_airpassengers, order=c(2,1,1)) # theo từng mô hình thay các order
arima_model
checkresiduals(arima_model)
acf(ts(arima_model$residuals))
pacf(ts(arima_model$residuals))

forecast_arima = forecast(arima_model, level = c(95), h=12*4)
plot(forecast_arima,                              # Draw train+forecast
     col = 2,
     xlab = "Number of passengers",
     ylab = "Month")
lines(test_airpassengers,                             # Draw test   
      col = 3)
legend("topright",                           # Add legend to plot
       c("Train", "Test", "Predict"),
       lty = 1,
       col = 2:4)

# Chạy mô hình ar
ar_model <- arima(train_airpassengers, order=c(1,0,0)) # theo từng mô hình thay các order
ar_model
checkresiduals(ar_model)
acf(ts(ar_model$residuals))
pacf(ts(ar_model$residuals))

forecast_ar = forecast(ar_model, level = c(95), h=12*4)
plot(forecast_ar,                              # Draw train+forecast
     col = 2,
     xlab = "Number of passengers",
     ylab = "Month")
lines(test_airpassengers,                             # Draw test   
      col = 3)
legend("topleft",                           # Add legend to plot
       c("Train", "Test", "Predict"),
       lty = 1,
       col = 2:4)

# Chạy mô hình arma
arma_model <- arima(train_airpassengers, order=c(2,0,1)) # theo từng mô hình thay các order
arma_model
checkresiduals(arma_model)
acf(ts(arma_model$residuals))
pacf(ts(arma_model$residuals))

forecast_arma = forecast(arma_model, level = c(95), h=12*4)
plot(forecast_arma,                              # Draw train+forecast
     col = 2,
     xlab = "Number of passengers",
     ylab = "Month")
lines(test_airpassengers,                             # Draw test   
      col = 3)
legend("topleft",                           # Add legend to plot
       c("Train", "Test", "Predict"),
       lty = 1,
       col = 2:4)


