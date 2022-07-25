#import library
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

# Load file
data_AirPassengers <- read.csv('./DATA/AirPassengers.csv')

# Overview data
summary(data_AirPassengers)

# Data clean


# Vẽ biểu đồ số lương khách hàng
AirPassengers <- ts(data_AirPassengers[-1], start = c(1949,1), end = c(1960,12), frequency =12)
plot(AirPassengers, ylab="AirPassengers", col='Blue')
lAirPassengers <- log(AirPassengers)
plot(lAirPassengers, ylab="log(AirPassengers)", col='red')

#Chia tập dữ liệu
sample <- seq_len(length.out = floor(x = 0.8 * nrow(x = data_AirPassengers)))
data_train <- data_AirPassengers[sample, ]
data_test  <- data_AirPassengers[-sample, ]

# Biểu đồ phân rã chuỗi thời gian
tAirPassengers <- ts(data_train[-1], start = c(1949,1), end = c(1958,7), frequency =12)
fit <- stl(tAirPassengers[,1], s.window="period")
plot(fit, main="A seasonal decomposition of the logged AirPassengers time series")
tsdata <- ts(tAirPassengers, frequency = 12) 
ddata <- decompose(tsdata, "multiplicative")
plot(ddata$trend)
plot(ddata$seasonal)
# Nhận xét sơ bộ: Dữ liệu có tính mùa vụ, thời gian càng tăng thì số lượng khách hàng càng tăng, Có xu hướng trong tương lai.

# Vẽ đường xu hướng trên tập dữ liệu
plot(tAirPassengers)
abline(reg=lm(tAirPassengers~time(tAirPassengers)))

# Thể hiện chu kỳ
monthplot(tAirPassengers, xlab="", ylab="", main="Month Plot", col = "blue")
seasonplot(tAirPassengers, year.labels="TRUE", main="Season Plot", col = 1:11)

#Tạo Box plot theo chu kỳ
boxplot(tAirPassengers~cycle(tAirPassengers, xlab="Date", ylab = "Passenger Numbers (1000's)", main = "Monthly air passengers boxplot from 1949-1960"))



# Hình ảnh cho kiểm định tính dừng của dữ liệu
plot(diff(log(tAirPassengers)))
acf(diff(log(tAirPassengers)))
# Tính q
pacf(diff(log(tAirPassengers)))
# Tính p
# Kiểm tra tính dừng của dữ liệu
#Không có level 
adf.test(tAirPassengers)
sales_ts_d1 <- diff(tAirPassengers, differences = 2)
adf.test(sales_ts_d1)

# Xây dựng mô hình ARIMA sử dụng auto.arima()Function
mymodel <- auto.arima(tAirPassengers, ic="aic", trace = TRUE)
mymodel
plot.ts(mymodel$residuals)


#Dự báo
contributions.fc <- meanf(tAirPassengers, h=4)
autoplot(contributions.fc)
summary(contributions.fc)
checkresiduals(contributions.fc)
# Dự báo giá trị cho 10 năm tới
myforecast <- forecast(mymodel, level=c(95), h=10*12)
plot(myforecast)

acf(ts(mymodel$residuals))
pacf(ts(mymodel$residuals))
mygdpforecast=forecast(mymodel, level = c(95),h=10*4)
mygdpforecast
plot(mygdpforecast)

# Xác thực mô hình bằng cách chọn giá trị trễ
Box.test(mymodel$resid, lag=5, type="Ljung-Box")
Box.test(mymodel$resid, lag=10, type="Ljung-Box")
Box.test(mymodel$resid, lag=15, type="Ljung-Box")



# Nháp  
pred <- forecast(fit,5) # predicting the next five values of the AirPassengers time series
pred
plot(pred, main="Forecast for Air Travel", ylab="Log(AirPassengers)", xlab="Time", col="blue")
# monthplot(pAirPassengers, xlab="", ylab="", main="Month Plot", col = "blue") 
# seasonplot(pAirPassengers, year.labels="TRUE", main="Season Plot", col = 1:11)
# Mô hình trên cho thấy rõ ràng hơn về tính mùa vụ của dữ liệu qua các năm.

# Chạy thử nghiệm 


