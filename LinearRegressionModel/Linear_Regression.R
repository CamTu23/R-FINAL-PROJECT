#import library
library(dplyr) #data manipulation
library(ggplot2) #data visualization
library(DT) #beautify the data table
library(plotly) #make a pie chart
library(Metrics) #find error value
library(Hmisc)
library(cowplot)

library(WVPlots)

library(pearson7)
library(reshape2)
library(fastDummies)
library(corrplot)
library(neuralnet)
library(MASS)
#Load file
data_insurance_1 <- read.csv("./DATA/insurance.csv")

#Understanding data
describe(data_insurance_1) #xem có biến nào bị null, không có giá trị không
head(data_insurance_1)
#EDA
#Correlation age and charge
ggplot(data_insurance_1, aes(age, charges, colour= age)) + geom_jitter(alpha = 0.5) + labs(title = "Correlation between age and charges")
#Correlation bmi and charge
ggplot(data_insurance_1, aes(bmi, charges, colour= bmi)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between BMI and charges")
#Correlation charge and sex
ggplot(data_insurance_1, aes(sex, charges, colour= sex)) + geom_jitter(alpha = 0.5) + labs(title = "Correlation between sex and charges")
#Correlation charge and Children
ggplot(data_insurance_1, aes(children, charges, colour= children)) + geom_jitter(alpha = 0.5) + labs(title = "Correlation between children and charges")

#Correlation charge and smoker
ggplot(data_insurance_1, aes(smoker, charges, colour= smoker)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between smoker and charges")

#Correlation charge and Region
ggplot(data_insurance_1, aes(region, charges, colour= region)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between region and charges")

# data_insurance = dummy_cols(data_insurance_1, select_columns = c("sex","smoker","region"))
# head(data_insurance)
# #options(max.print=5000)
# str(data_insurance)
# 
# #Xoa cot chu Dl 
# data_insurance <- data_insurance[,-c(2,5,6)]
# head(data_insurance)

#đổi DL
data_insurance_1$sex=ifelse(data_insurance_1$sex=='female',1,0)
data_insurance_1$smoker=ifelse(data_insurance_1$smoker=='yes',1,0)
data_insurance_1$region=ifelse(data_insurance_1$region=='southwest',1,ifelse(data_insurance_1$region=='southeast',2,ifelse(data_insurance_1$region=='northwest',3,4)))
str(data_insurance_1)

data_insurance <- as.data.frame(data_insurance_1)
cor(data_insurance)
corrplot(cor(data_insurance), method = "circle")

# data_insurance$sex = as.factor(data_insurance$sex)
# data_insurance$smoker = as.factor(data_insurance$smoker)
# data_insurance$region = as.factor(data_insurance$region)
# data_insurance_2 <- melt(data_insurance)
# head(data_insurance_2)
# ggplot(data = data_insurance, aes(x=, y=, fill=value)) + geom_tile()

#Sau khi biểu diễn độ tương quan giữa charges và các biến thì ta thấy rằng:
# - Đối với children, ngoại trừ không có trẻ em thì có mật độ giống nhau. Chưa có con sẽ ảnh hưởng đến chi phí bảo hiểm
# - Đối với age tuổi càng cao chi phí bảo hiểm càng cao
# - Đối với bmi, tương quan khá rắc rối, cmi càng cao chi phí càng cao
# - Đối với giới tính, không có sự tương quan gì giữa giới tính và chi phí bảo hiểm
# - Đối với smoker, có sự phụ thuộc của người hút thuốc, người hút thuốc thì sẽ có chi phí bảo hiểm cao hơn không hút thuốc
# - Đối với region, không có sự tương quan nào, chi phí bảo hiểm không phụ thuộc vào vùng miền 

#=> chọn được age, bmi, children, smoker

#So sánh độ tương quan giữa các biến độc lập với nhau
#Age & BMI, không có sự tương quan với nhau

# ggplot(data_insurance, aes(bmi, age, colour= bmi)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between BMI and age")
# # #Age & smoker, không có sự tương quan với nhau
# ggplot(data_insurance, aes(smoker, age, colour= smoker)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between smoker and age")
# # #Age & Children không có sự tương quan với nhau
# ggplot(data_insurance, aes(children, age, colour= children)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between children and age")
# # #BMI & Smoker, không có sự tương quan với nhau
# ggplot(data_insurance, aes(smoker, bmi, colour= smoker)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between smoker and bmi")
# # # BMI & Children không có sự tương quan với nhau
# ggplot(data_insurance, aes(children, bmi, colour= bmi)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between children and bmi")
# # 
# # # => Sau khi xem sự tương quan giữa các biến độc lập => Biểu đồ rắc rối, không nhận thấy rõ được sự tương quan hay không tương quan giữa các biến độc lập

#
# formula_1 <- as.formula("charges ~ age") #mô hình 1 biến phụ thuộc 2 biến độc lập
# model_1 <- lm(formula_1, data = data_train) #hàm lm() cho phép thực hiện hồi quy tuyến tính
# summary(model_1)
# #
# formula_2 <- as.formula("charges ~ age") #mô hình 1 biến phụ thuộc 2 biến độc lập
# model_2 <- lm(formula_2, data = data_test) #hàm lm() cho phép thực hiện hồi quy tuyến tính 
# summary(model_2)

# #don bien
# summary(lm(formula = charges ~ age, data = data_insurance))
# summary(lm(formula = charges ~ bmi, data = data_insurance))
# # Nếu là mô hình đơn biến => chọn age vì R - square của mô hình biến độc lập age lớn hơn
# # => Mô hình hồi quy đơn biến là: 3165.9
# 
# #da bien
# summary(lm(formula = charges ~ age + bmi, data = data_insurance))
# #R - square = 0.11593
set.seed(123)
# Split data to train and test
train <- round(0.8 * nrow(data_insurance))
# từ dataet gốc, lấy 80% bộ dữ liệu -> train_indices
train_indices <- sample(1:nrow(data_insurance),train)
#sau khi có train_indices, lúc này vẫn chưa là 1 bảng -> phải chuyển đổi thành bảng
data_train <- data_insurance[train_indices, ]
#phần còn lại 20% là test
data_test <- data_insurance[-train_indices, ]


#da bien
formula_1 <- as.formula("charges ~ bmi+children + smoker")
model_1 <- lm(formula_1, data = data_train)
summary(model_1) # Y = -4652.07 + 401.65*bmi + 658.48*children + 23904.22*smoker

#don bien
formula_2 <- as.formula("charges ~ smoker") # Smoker có R-squared lớn nhất = 63.45% nên sẽ chọn biến này biến độc lập cho mô hình linear đơn biến
model_2 <- lm(formula_2, data = data_train)
summary(model_2)

formula_3 <- as.formula("charges ~ children")
model_3 <- lm(formula_3, data = data_train)
summary(model_3)

formula_4 <- as.formula("charges ~ bmi")
model_4 <- lm(formula_4, data = data_train)
summary(model_4)


# Model Performance 
# Prediction vs. Real values hình 1
data_test$prediction <- predict(model_1, newdata = data_test)
ggplot(data_test, aes(prediction, charges)) + geom_point(color = "blue", alpha = 0.5) + geom_abline(color = "red") + ggtitle("Prediction vs. Real values")
summary(data_test$prediction)
# => Dự đoán đoạn đầu giá trị thực nằm trên trùng khớp với đường dự đoán, đoạn sau các giá trị thực nằm trên dưới đường dự đoán 1 đoạn, giá trị dự đoán không chính xác
# Dự đoán hình 2
data_test$residuals <- data_test$charges - data_test$prediction
ggplot(data = data_test, aes(x = prediction, y = residuals)) + geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 3, color = "red") + ggtitle("Residuals vs. Linear model prediction")

# cũng như chart trên
GainCurvePlot(data_test, "prediction", "charges", "Model")
# đường giá trị thực nằm trên giá trị dự đoán
# predict với giá trị thực
Tu <- data.frame(age = 20,
                  bmi = 27.9,
                  sex = 1,
                  children = 0,
                  smoker = 1,
                  region = 4)
print(paste0("Health care charges for Tu: ", round(predict(model_1, Tu), 2))) #30458.26
# giá trị thực 2
Tu <- data.frame(age = 20,
                 bmi = 27.9,
                 sex = 0,
                 children = 0,
                 smoker = 0,
                 region = 3)
print(paste0("Health care charges for Tu: ", round(predict(model_1, Tu), 2))) #6554.04


#Machine learning
#neural network
# Build Neural Network
nn <- neuralnet(charges ~ age + bmi + sex + children + smoker + region, 
                data = data_train, hidden = c(5, 3), 
                linear.output = TRUE)

# Predict on test data
pr.nn <- compute(nn, data_test[,1:7])

# Compute mean squared error
pr.nn_ <- pr.nn$net.result * (max(data$charges) - min(data$charges)) 
+ min(data$charges)
test.r <- (data_test$charges) * (max(data$charges) - min(data$charges)) + 
  min(data$charges)
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(data_test)

# Plot the neural network
plot(nn)







