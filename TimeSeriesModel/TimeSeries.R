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

# # Sự phân rã theo mùa, theo xu hướng, theo dữ liệu, theo phần còn lại của dữ liệu
# decompose_data <- stl(log_air_passengers[,1], s.window="period") #hàm stl phân rã chuỗi thời gian, phân tích theo mùa, xu hướng bằng cách sử dụng LOESS (Local Regression)
# plot(decompose_data, main="Phân rã chuỗi thời gian theo mùa, theo xu hướng", col = "blue")
# 
# head(fit$time.series) # các đối tượng của các biến quan sát
# head(exp(fit$time.series)) # chuyển đổi sự phân rã trở lại số liệu ban đầu
# # Hình dung sự phân rã theo tháng theo năm
# monthplot(airpassengers, xlab="Month", ylab="Passenger", main="Month Plot", col = "blue") #Cho thấy số lượng hành khách biến động theo từng tháng và số lượng trung bình hành khách theo từng tháng, cao nhất ở tháng 6,7,8. Nhìn chung số lượng hành khách mỗi tháng tăng dần đều.
# seasonplot(airpassengers, year.labels="TRUE", main="Season Plot", col = 1:11) # tăng dần theo năm 


# Split data to train and test

train_air_indices <- seq_len(length.out = floor(x = 0.8 * nrow(x = data_airpassenger)))
train_air <- data_airpassenger[train_air_indices, ]
test_air <- data_airpassenger[-train_air_indices, ]

#Phân rã
train_airpassengers <- ts(train_air[-1], start = c(1949,1), end = c(1958,7), frequency =12) # frequency = 12 do data có cột month là dữ liệu hầng tháng qua các năm
log_train_airpassenger <- log(train_airpassengers)
# decompose_train_data <- stl(log_train_airpassenger[,1], s.window = "period") #hàm stl phân rã chuỗi thời gian, phân tích theo mùa, xu hướng bằng cách sử dụng LOESS (Local Regression)

head(fit$time.series) # các đối tượng của các biến quan sát
head(exp(fit$time.series)) # chuyển đổi sự phân rã trở lại số liệu ban đầu
# Hình dung sự phân rã theo tháng theo năm
monthplot(train_airpassengers, xlab="Month", ylab="Passenger", main="Month Plot", col = "blue") #Cho thấy số lượng hành khách biến động theo từng tháng và số lượng trung bình hành khách theo từng tháng, cao nhất ở tháng 6,7,8. Nhìn chung số lượng hành khách mỗi tháng tăng dần đều.
seasonplot(train_airpassengers, year.labels="TRUE", main="Season Plot", col = 1:11) # tăng dần theo năm

# Kiểm tra dữ liệu có dừng không?

adf.test(diff(log_train_airpassenger)) # 0.01 smaller -> non-stationary  
kpss.test(diff(log_train_airpassenger)) # 0.1 > -> non - stationary

#CHUYỂN DỮ LIỆU VỀ DỪNG

#KIỂM ĐỊNH TỰ TƯƠNG QUAN
acf(diff(log_train_airpassenger)) # q
pacf(diff(log_train_airpassenger)) # p
#Create model
arima_model_air = auto.arima(diff(log_train_airpassenger), ic = "aic", trace = TRUE)
