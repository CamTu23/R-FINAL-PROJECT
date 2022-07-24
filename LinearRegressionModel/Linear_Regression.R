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
#Load file
data_insurance <- read.csv("./DATA/insurance.csv")

#Understanding data
describe(data_insurance) #xem có biến nào bị null, không có giá trị không

#EDA
#Correlation age and charge
ggplot(data_insurance, aes(age, charges, colour= age)) + geom_jitter(alpha = 0.5) + labs(title = "Correlation between age and charges")
#Correlation bmi and charge
ggplot(data_insurance, aes(bmi, charges, colour= bmi)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between BMI and charges")
#Correlation charge and sex
ggplot(data_insurance, aes(sex, charges, colour= sex)) + geom_jitter(alpha = 0.5) + labs(title = "Correlation between sex and charges")
#Correlation charge and Children
ggplot(data_insurance, aes(children, charges, colour= children)) + geom_jitter(alpha = 0.5) + labs(title = "Correlation between children and charges")

#Correlation charge and smoker
ggplot(data_insurance, aes(smoker, charges, colour= smoker)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between smoker and charges")

#Correlation charge and Region
ggplot(data_insurance, aes(region, charges, colour= region)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between region and charges")

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
ggplot(data_insurance, aes(bmi, age, colour= bmi)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between BMI and age")
# #Age & smoker, không có sự tương quan với nhau
ggplot(data_insurance, aes(smoker, age, colour= smoker)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between smoker and age")
# #Age & Children không có sự tương quan với nhau
ggplot(data_insurance, aes(children, age, colour= children)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between children and age")
# #BMI & Smoker, không có sự tương quan với nhau
ggplot(data_insurance, aes(smoker, bmi, colour= smoker)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between smoker and bmi")
# # BMI & Children không có sự tương quan với nhau
ggplot(data_insurance, aes(children, bmi, colour= bmi)) + geom_jitter(alpha = 0.7) + labs(title = "Correlation between children and bmi")
# 
# # => Sau khi xem sự tương quan giữa các biến độc lập => Biểu đồ rắc rối, không nhận thấy rõ được sự tương quan hay không tương quan giữa các biến độc lập

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
# #R - square = 0.1159

# Split data to train and test
train <- round(0.8 * nrow(data_insurance))
# từ dataet gốc, lấy 80% bộ dữ liệu -> train_indices
train_indices <- sample(1:nrow(data_insurance),train)
#sau khi có train_indices, lúc này vẫn chưa là 1 bảng -> phải chuyển đổi thành bảng
data_train <- data_insurance[train_indices, ]
#phần còn lại 20% là test
data_test <- data_insurance[-train_indices, ]


#da bien
formula_1 <- as.formula("charges ~ age + bmi + children + smoker")
model_1 <- lm(formula_1, data = data_train)
summary(model_1) # Y = -12130.4 + 257.2*X1 + 326.2*X2 + 514.3*X3 + 23576.1*X4 

# r_sq_1 <- summary(model_1)$r.squared
# prediction_1 <- predict(model_1, newdata = data_test)
# residuals_1 <- data_test$charges - prediction_1
# rmse_1 <- sqrt(mean(residuals_1^2))
# print(paste0("R-squared for new model: ", round(r_sq_1, 4)))

#don bien
formula_2 <- as.formula("charges ~ smoker")
model_2 <- lm(formula_2, data = data_train)
summary(model_2)


# #Cat - 1: Smoker, have no dependent, BMI under 30.
# ins_smoker_nochild_under30 <- data_insurance %>% filter(smoker == "yes" & children == 0 & bmi < 30)
# ggplot(ins_smoker_nochild_under30, aes(age, charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với chi Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_under30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và chi Phí có vẻ có mối tương quan rối loạn.
# summary(lm(charges ~ age, data = ins_smoker_nochild_under30)) # R = 0.4725
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_under30)) # R = 0.5264
# # Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# # -956.74 + (251.20 * AGE) + (505.18 * BMI)
# 
# #Cat - 2: Smoker, have no dependent, BMI over 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "yes" & children == 0 & bmi >= 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# 
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan, BMI tăng thì phí tắng. Có thể tiếp cận giao diện biểu đồ BMI so với Phí bằng cách sử dụng hồi quy tuyến tính.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) # R = 0.4495
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.565
# # Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# # 8120.10 + (292.16 * AGE) + (614.01 * BMI)
# 
# # Cat - 3: Smoker, have dependents, BMI under 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "yes" & children > 0 & bmi >= 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan, BMI tăng thì phí tắng. Có thể tiếp cận giao diện biểu đồ BMI so với Phí bằng cách sử dụng hồi quy tuyến tính.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) # R = 0.4043
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.553
# # Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# # 2428.48 + (259.48 * AGE) + (359.27 * BMI)
# 
# # Cat - 4 Smoker, have dependents, BMI over 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "yes" & children == 0 & bmi >= 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan, BMI tăng thì phí tắng. Có thể tiếp cận giao diện biểu đồ BMI so với Phí bằng cách sử dụng hồi quy tuyến tính.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) # R = 0.4495
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.565
# # Từ hai bản tóm tắt thì quyết định sử dụng hồi quy tuyến tính với 2 biến (tuổi và BMI) vì nó có giá trị R-Squared tốt hơn.
# # 16021.03 + (253.72 * AGE) + (447.91 * BMI)
# 
# #  Cat - 5 Non-smoker, have no dependent, BMI under 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children == 0 & bmi < 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) # R = 0.5543
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.5524
# # Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# # -3239.15 + (277.00 * AGE)
# 
# # Cat - 6: Non-smoker, have no dependent, BMI over 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children == 0 & bmi >= 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) # R = 0.556
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.5552
# # Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# # -2155.79 + (254.01 * AGE)
# 
# # Cat - 7: Non-smoker, have dependents, BMI under 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children > 0 & bmi < 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) #R = 0.2436
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.2427
# # Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# # -884.08 + (247.85 * AGE)
# 
# # Cat - 8: Non-smoker, have dependents, BMI over 30.
# ins_smoker_nochild_over30 <- data_insurance %>% filter(smoker == "no" & children > 0 & bmi >= 30)
# ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()
# # Tuổi càng cao thì phí càng cao. Có thể tiếp cận giao diện biểu đồ Tuổi so với Phí bằng cách sử dụng hồi quy tuyến tính.
# ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()
# # Chỉ số BMI và Phí có vẻ có mối tương quan rối loạn.
# summary(lm(charges ~ age, data = ins_smoker_nochild_over30)) # R = 0.296
# summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30)) # R = 0.2944
# # Từ hai bản tóm tắt đó, tôi quyết định sử dụng hồi quy tuyến tính với 1 biến (tuổi) vì nó có giá trị R-Squared tốt hơn.
# # -2161.36 + (282.54 * AGE)
# 
# # chốt đa là biến age, bmi
# # Chốt đơn biến là age
# 
# 
# # Split data to train and test
# train <- round(0.8 * nrow(data_insurance))
# # từ dataet gốc, lấy 80% bộ dữ liệu -> train_indices
# train_indices <- sample(1:nrow(data_insurance),train)
# #sau khi có train_indices, lúc này vẫn chưa là 1 bảng -> phải chuyển đổi thành bảng
# data_train <- data_insurance[train_indices, ]
# #phần còn lại 20% là test
# data_test <- data_insurance[-train_indices, ]
# 
# 
# # Xây dựng model
# summary(lm(charges ~ age, data = data_train)) # Insurance_cost = 3165.9 + 257.7*age
# summary(lm(charges ~ age + bmi, data = data_train)) # Insurance_cost = -6424.8 + 241.93*age + 332.97*bmi

# Model Performance 
# Prediction vs. Real values hình 1
data_test$prediction <- predict(model_1, newdata = data_test)
ggplot(data_test, aes(prediction, charges)) + geom_point(color = "blue", alpha = 0.5) + geom_abline(color = "red") + ggtitle("Prediction vs. Real values")
# => Dự đoán đoạn đầu giá trị thực nằm trên trùng khớp với đường dự đoán, đoạn sau các giá trị thực nằm trên dưới đường dự đoán 1 đoạn, giá trị dự đoán không chính xác
# Dự đoán hình 2
data_test$residuals <- data_test$charges - data_test$prediction
ggplot(data = data_test, aes(x = prediction, y = residuals)) + geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 3, color = "red") + ggtitle("Residuals vs. Linear model prediction")
# cũng như chart trên
GainCurvePlot(data_test, "prediction", "charges", "Model")
# đường giá trị thực nằm trên giá trị dự đoán






