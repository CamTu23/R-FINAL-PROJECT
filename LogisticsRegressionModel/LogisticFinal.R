###############################
#Machine learning
library(party)
library(rpart) #decision tree
library(rpart.plot)
dt.df <- DL
head(dt.df)
dt.df <- data.frame(dt.df)

set.seed(8)
train.size = floor(0.75*nrow(dt.df))
train.index = sample(1:nrow(dt.df), train.size)
train.set = dt.df[train.index,]
test.set = dt.df[-train.index,]

#Xây dựng model decision tree
model_tree <- rpart(y ~., train.set)
rpart.plot(model_tree)

# Dự báo và tính toán sai số trên mẫu kiểm định (test.set)
# type = class dự báo là 0 hoặc 1 (không hoặc có)
Prediction.dt = predict(model_tree, test.set,type = "class")
table(Predicted = Prediction.dt, True = test.set$y)
matrix_1 = table(Predicted = Prediction.dt, True = test.set$y)
# Độ chính xác trên mẫu kiểm định
(matrix_1[1,1] + matrix_1[2,2])/(nrow(test.set))
# => Độ chính xác của mô hình là 89.84%

#####################
#Random Forest
library(randomForest)
library(caret)
set.seed(8)
rf.df <- DL
head(rf.df)
rf.df <- data.frame(rf.df)

#Chia bộ mẫu train/test
train.size = floor(0.75*nrow(rf.df))
train.index = sample(1:nrow(rf.df), train.size)
train.set = rf.df[train.index,]
test.set = rf.df[-train.index,]
y.test = rf.df[-train.index,17]

#Xây dựng model randomForest
#Sử dụng as.factor (x) trên danh sách các giá trị phân loại (list of categorical integer values)
rf.model <- randomForest(as.factor(y) ~ ., data=train.set)
print(rf.model)

#Evaluate variable importance
importance(rf.model)
# => Dựa vào gini, gini càng lớn thì biến đó càng quan trọng, biến quan trọng nhất là duration, tiếp đến là balance, age, day, month,...
# Vẽ biểu đồ đánh giá các biến độc lập
varImpPlot(rf.model,cex = 1)

#Dự báo mô hình
Prediction.rf = predict(rf.model, data = test.set, type = "class")

#Ma trận hỗn loạn
table(Predicted = Prediction.rf, True = test.set$y)
matrix_1 = table(Predicted = Prediction.rf, True = test.set$y)

#Độ chính xác trên mẫu kiểm định
(matrix_1[1,1] + matrix_1[2,2])/(nrow(test.set))

###################

