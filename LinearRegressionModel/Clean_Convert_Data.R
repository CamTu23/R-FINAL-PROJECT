#import library
library(dplyr) #data manipulation
library(ggplot2) #data visualization
library(DT) #beautify the data table
library(plotly) #make a pie chart
library(Metrics) #find error value
library(Hmisc)
library(cowplot)
library(WVPlots)


#Load file
d <- file.choose()
data_insurance <- read.csv(d)


#Understanding data
describe(data_insurance)


#EDA
#Correlation charge and age
ggplot(data_insurance, aes(age, charges)) + geom_jitter(color = "red", alpha = 0.5)
#Khi Tuổi tăng Phí bảo hiểm y tế cũng có xu hướng tăng.
#Correlation charge and bmi
ggplot(data_insurance, aes(bmi, charges)) + geom_jitter(color = "green", alpha = 0.5)
# Khi BMI tăng Phí bảo hiểm y tế cũng có xu hướng tăng.
# Không có mối liên hệ rõ ràng nào giữa Phí bảo hiểm và Tuổi.
#Correlation charge and sex
ggplot(data_insurance, aes(sex, charges)) + geom_jitter(aes(color = sex), alpha = 0.5)
# Không có kết nối rõ ràng giữa Phí và giới tính.
#Correlation charge and Children
ggplot(data_insurance, aes(children, charges)) + geom_jitter(aes(color = children), alpha = 0.5)
#Phí bảo hiểm cho 4-5 trẻ em được bảo hiểm dường như giảm xuống (không có ý nghĩa gì). Tuy nhiên, nếu bạn chưa có con, điều đó sẽ ảnh hưởng đến bảo hiểm chi phí y tế.
#Correlation charge and smoker
ggplot(data_insurance, aes(smoker, charges)) + geom_jitter(aes(color = smoker), alpha = 0.5)
# Phí cho người hút thuốc cao hơn cho người không hút thuốc (không có gì ngạc nhiên ở đây).
#Correlation charge and Region
ggplot(data_insurance, aes(region, charges)) + geom_jitter(aes(color = region), alpha = 0.5)
# Không có kết nối rõ ràng giữa Phí và Khu vực.

# Biến cần xét: age, bmi, children, smoker



#Cat - 1: Smoker, have no dependent, BMI under 30.
ins_smoker_nochild_under30 <- data_insurance %>% filter(smoker == "yes" & children == 0 & bmi < 30)
ggplot(ins_smoker_nochild_under30, aes(age, charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_under30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
summary(lm(charges ~ age, data = ins_smoker_nochild_under30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_under30))
# Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# -956.74 + (251.20 * AGE) + (505.18 * BMI)

#Cat - 2: Smoker, have no dependent, BMI over 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "yes" & children == 0 & bmi >= 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan, BMI tăng thì phí tắng. Có thể tiếp cận giao diện biểu đồ BMI so với Phí bằng cách sử dụng hồi quy tuyến tính.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# 8120.10 + (292.16 * AGE) + (614.01 * BMI)

# Cat - 3: Smoker, have dependents, BMI under 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "yes" & children > 0 & bmi >= 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan, BMI tăng thì phí tắng. Có thể tiếp cận giao diện biểu đồ BMI so với Phí bằng cách sử dụng hồi quy tuyến tính.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# 2428.48 + (259.48 * AGE) + (359.27 * BMI)

# Cat - 4 Smoker, have dependents, BMI over 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "yes" & children == 0 & bmi >= 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan, BMI tăng thì phí tắng. Có thể tiếp cận giao diện biểu đồ BMI so với Phí bằng cách sử dụng hồi quy tuyến tính.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# 16021.03 + (253.72 * AGE) + (447.91 * BMI)

#  Cat - 5 Non-smoker, have no dependent, BMI under 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children == 0 & bmi < 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# -3239.15 + (277.00 * AGE)

# Cat - 6: Non-smoker, have no dependent, BMI over 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children == 0 & bmi >= 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# -2155.79 + (254.01 * AGE)

# Cat - 7: Non-smoker, have dependents, BMI under 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children > 0 & bmi < 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
ggplot(ins_smoker_nochild_over30, aes(x = charges, y = smoker)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# -884.08 + (247.85 * AGE)

# Cat - 8: Non-smoker, have dependents, BMI over 30.
ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children > 0 & bmi >= 30)
ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
summary(lm(charges ~ age, data = ins_smoker_nochild_over30))
# Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# -2161.36 + (282.54 * AGE)
# chốt đa là biến age, bmi
# Chốt đơn biến là age

## Linear Regression Model
### Preparation and splitting the data
n_train <- round(0.8 * nrow(data_insurance))
train_indices <- sample(1:nrow(data_insurance), n_train)
Data_train <- data_insurance[train_indices, ]
Data_test <- data_insurance[-train_indices, ]

#Train and Test the Model
formula_0 <- as.formula("charges ~ age + bmi")
model_0 <- lm(formula_0, data = Data_train)
summary(model_0)
#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared
#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

# Train and Test New Model
formula_1 <- as.formula("charges ~ age + bmi")
model_1 <- lm(formula_1, data = Data_train)
summary(model_1)
r_sq_1 <- summary(model_1)$r.squared
prediction_1 <- predict(model_1, newdata = Data_test)
residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))

#In ra số liệu để so sánh
print(paste0("R-squared for first model:", round(r_sq_0, 4)))
print(paste0("R-squared for new model: ", round(r_sq_1, 4)))
print(paste0("RMSE for first model: ", round(rmse_0, 2)))
print(paste0("RMSE for new model: ", round(rmse_1, 2)))
#Chốt là dùng cái New Model vì hiệu suất khá giống nhau giữa hai mô hình và nó đơn giản hơn một chút.

# Model Performance 
# Prediction vs. Real values hình 1
Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(prediction, charges)) + geom_point(color = "blue", alpha = 0.5) + geom_abline(color = "red") + ggtitle("Prediction vs. Real values")
# Dự đoán hình 2
Data_test$residuals <- Data_test$charges - Data_test$prediction
ggplot(data = Data_test, aes(x = prediction, y = residuals)) + geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 3, color = "red") + ggtitle("Residuals vs. Linear model prediction")

GainCurvePlot(Data_test, "prediction", "charges", "Model")


# Đánh giá mô hình
summary(lm(charges ~ age, data = Data_test))
summary(lm(charges ~ age +  ta = Data_test))


















