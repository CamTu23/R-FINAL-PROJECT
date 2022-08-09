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
library(e1071)
library(caret)
library(rpart) #decision tree
library(rpart.plot)
library(party)
library(randomForest)
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

# Chọn ra được biến children, bmi, smoke
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
                 sex = 1,
                 children = 0,
                 smoker = 0,
                 region = 4)
print(paste0("Health care charges for Tu: ", round(predict(model_1, Tu), 2))) #6554.04


#Machine learning
#neural network
# Build Neural Network
# nn <- neuralnet(charges ~ bmi + children + smoker, 
#                 data = data_train, hidden = c(5, 3), 
#                 linear.output = TRUE)
# 
# # Predict on test data
# pr.nn <- compute(nn, data_test[,1:7])
# 
# # Compute mean squared error
# pr.nn_ <- pr.nn$net.result * (max(data$charges) - min(data$charges)) 
# + min(data$charges)
# test.r <- (data_test$charges) * (max(data$charges) - min(data$charges)) + 
#   min(data$charges)
# pred_nn = predict(nn, data_test)
# MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(data_test)
# mae = MAE( data_test$charges, pred_nn)
# rmse = RMSE( data_test$charges, pred_nn)
# r2 = R2( data_test$charges, pr_nn, form = "traditional")
# cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)
# 
# # Plot the neural network
# plot(nn)
# 
# plot(data_test$charges, pr.nn_, col = "red", 
#      main = 'Real vs Predicted')
# abline(0, 1, lwd = 2)

#SVM
#Xây dựng mô hình và dự đoán
model_reg = svm(charges~., data= data_train)
print(model_reg)
pred = predict(model_reg, data_test)
x = 1:length(data_test$charges)
plot(x, data_test$charges, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")
# Kiểm tra độ chính xác
mae = MAE( data_test$charges, pred)
rmse = RMSE( data_test$charges, pred)
r2 = R2( data_test$charges, pred, form = "traditional")
cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)

#decision tree
model_tree<-rpart(charges ~., data = data_train)
rpart.plot(model_tree)
#Bảng Giá trị predict
tbl<-table(predict(model_tree), data_train$charges)
print(tbl)
#Predict và đánh giá
pred_tree<-predict(model_tree,data_test)
mae = MAE( data_test$charges, pred_tree)
rmse = RMSE( data_test$charges, pred_tree)
r2 = R2( data_test$charges, pred_tree, form = "traditional")
cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)

# Random forest
random_forest <- randomForest(charges ~., data = data_train, mtry = 3, importance = TRUE, na.action = na.omit)
print(random_forest)
plot(random_forest)
pred_forest<-predict(random_forest,data_test)
mae = MAE( data_test$charges, pred_forest)
rmse = RMSE( data_test$charges, pred_forest)
r2 = R2( data_test$charges, pred_forest, form = "traditional")
cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)

