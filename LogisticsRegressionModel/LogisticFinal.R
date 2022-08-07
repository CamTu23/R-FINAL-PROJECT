library(readr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(caret)
library(MASS)
library(tidyverse)
library(mlbench)
library(gmodels)
library(class)
library(epiDisplay)
library(DescTools)
library(rms)
library(ROCR)
library(glmnet)
library(caTools)
library(InformationValue)
library(magrittr)
library(fastDummies)
library(plotly)
library(GGally)
library(ggcorrplot)

f <- file.choose() 
DL <- read.csv(f) 
train.dl <- DL
train.dl <- data.frame(train.dl)

str(train.dl)
summary(train.dl)
#kham pha dl
# KT và loai bo gia tri thieu
any(is.null(train.dl))
any(is.na(train.dl))
describe(train.dl)
train.dl <- train.dl[complete.cases(train.dl),]

#truc quan
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

##biến
#DL
fig(12, 8)
ggplot(train.dl, aes(y))+ geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=10))
round(prop.table(table(train.dl$y))*100,1)
ggplot(train.dl, aes(default))+geom_bar(color = "black",fill = "khaki") + theme(text = element_text(size=10))
round(prop.table(table(train.dl$default))*100,1)
ggplot(train.dl, aes(housing)) + geom_bar(color = "black",fill = "firebrick2") + theme(text = element_text(size=10))
round(prop.table(table(train.dl$housing))*100,1)
ggplot(train.dl, aes(loan)) + geom_bar(color = "black",fill = "darkviolet") + theme(text = element_text(size=10))
round(prop.table(table(train.dl$loan))*100,1)

#kt phan phoi bien lien tuc
fig(8, 8)
ggplot(data = train.dl, aes(age, color = y))+ geom_freqpoly(binwidth = 5, size = 1)
summary(train.dl$age)
ggplot(data = train.dl, aes(balance, color = y))+ geom_freqpoly(binwidth = 1000, size = 1)
summary(train.dl$balance)
ggplot(data = train.dl, aes(duration, color = y))+ geom_freqpoly(binwidth = 100, size = 1)
summary(train.dl$duration)
ggplot(data = train.dl, aes(campaign, color = y))+ geom_freqpoly(binwidth = 5, size = 1)
summary(train.dl$campaign)
ggplot(data = train.dl, aes(day, color = y))+ geom_freqpoly(binwidth = 5, size = 1)
summary(train.dl$day)
ggplot(data = train.dl, aes(pdays, color = y))+ geom_freqpoly(binwidth =100, size = 1)
summary(train.dl$pdays)
ggplot(data = train.dl, aes(previous, color = y))+ geom_freqpoly(binwidth = 100, size = 1)
summary(train.dl$previous)

# Correlation matrix
cordata = train.dl[,c("age","balance","day","duration","campaign","pdays","previous")]
# Plot
corr <- round(cor(cordata), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of variables")

cor(cordata)
# Nhận xét có sự tương quan giữa các biến day, campain, pdays. Còn lại balance hoặc age vs  duration,previous k có sự tương quan nhưng xét tương quan thì age nhỏ hơn nên họn age
# Các biến còn lại: age,duration,previous

#### Categorical Variables
table(train.dl[("job")])
table(train.dl[("marital")])
table(train.dl[("education")])
table(train.dl[("poutcome")])
table(train.dl[('contact')])
table(train.dl[('month')])

fig(20,16)
ggplot(train.dl, aes(job)) + geom_bar(aes(x = job, fill = y)) +  theme(text = element_text(size=10), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
round(prop.table(table(train.dl$job))*100,1)
ggplot(train.dl, aes(marital)) + geom_bar(aes(x = marital, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$marital))*100,1)
ggplot(train.dl, aes(education))+ geom_bar(aes(x = education, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$education))*100,1)
ggplot(train.dl, aes(poutcome)) + geom_bar(aes(x = poutcome, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$poutcome))*100,1)
ggplot(train.dl, aes(contact)) + geom_bar(aes(x = contact, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$contact))*100,1)
ggplot(train.dl, aes(month))+ geom_bar(aes(x = month, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$month))*100,1)

###Logistic 
###Processing
lr.df <- train.dl
head(lr.df)
summary(lr.df)

# Xóa giá trị đa cọng 
##tạo các cột giả (nhị phân) từ các cột kiểu ký tự
# library(fastDummies)

lr.df = dummy_cols(lr.df, select_columns = c("job","marital","education","contact","month","poutcome"))
head(lr.df)
#options(max.print=5000)

#Xoa cot chu Dl 
lr.df <- lr.df[,-c(2,3,4,9,11,16,18,24,27,29,30,40,47,6,10,13,14)]
head(lr.df)
str(lr.df)
#đổi DL
lr.df$default=ifelse(lr.df$default=='yes',1,0)
lr.df$housing=ifelse(lr.df$housing=='yes',1,0)
lr.df$loan=ifelse(lr.df$loan=='yes',1,0)
lr.df$y = ifelse(lr.df$y == 'yes', 1, 0)
str(lr.df)

#chia bo mau 

set.seed(8)
train.size = floor(0.75*nrow(lr.df))
train.index = sample(1:nrow(lr.df), train.size)
train.set = lr.df[train.index,]
test.set = lr.df[-train.index,]

##library(dplyr)
##library(magrittr)
train.set %>%
  summarise(Total = n())
test.set %>%
  summarise(Total = n())

# Đơn biến: Xét biến loan 
logit_don =glm(y ~ housing, data = train.set,family = binomial(link = "logit"))
summary(logit_don)
# Mô hình có giá trị thống kê 
# dụ báo 
Prediction_train_don = predict(logit_don, newdata =  train.set, type = "response")
summary(Prediction_train_don)
Prediction_don  = predict(logit_don, newdata =  test.set, type = "response")
summary(Prediction_don)

# mức cắt dự đoán tối uu cho mô hình
optCutOff <- optimalCutoff(test.set$y, Prediction_don)[1] 
print(optCutOff)

misClassError(test.set$y, Prediction_don, threshold = optCutOff)

#xét sụ phù hợp
Concordance(test.set$y, Prediction_don)

# tính toán độ nhạy 
sensitivity(test.set$y, Prediction_don, threshold = optCutOff)

# tính toán độ đặc hiệu 
specificity(test.set$y, Prediction_don, threshold = optCutOff)

# xây dựng ma trận nhầm lẫn 
confusionMatrix(test.set$y,Prediction_don, threshold = optCutOff)
# Độ chính xác của mô hình
(5714+848)/nrow(test.set)
# Độ chính xác của mô hình co so 
(5714+515)/nrow(test.set)

#vẽ ROC
plotROC(test.set$y, Prediction_don)

#mô hình hồi quy đa biến Logistic trên tất cả các biến 

#xây dựng mô hình dựu báo trên bộ mẫu train.set
logit_reg =glm(y ~ ., data = train.set,family = binomial(link = "logit"))
summary(logit_reg) #chẩn đoán mô hình 

#dự báo trên bộ train.set 
Prediction_train = predict(logit_reg, newdata =  train.set, type = "response")
summary(Prediction_train)
# dự báo trên bộ test.set
Prediction = predict(logit_reg, newdata =  test.set, type = "response")
summary(Prediction)

# mức cắt dự đoán tối uu cho mô hình

optCutOff <- optimalCutoff(test.set$y, Prediction)[1] 
print(optCutOff)

##Model Diagnostics
#tính toán tổng tỷ lệ lỗi phân loại sai
misClassError(test.set$y, Prediction, threshold = optCutOff)
#ROC 
# library(ROCR)
# library(InformationValue)
#vẽ ROC
plotROC(test.set$y, Prediction)
#xét sụ phù hợp
Concordance(test.set$y, Prediction)

# tính toán độ nhạy 
sensitivity(test.set$y, Prediction, threshold = optCutOff)

# tính toán độ đặc hiệu 
specificity(test.set$y, Prediction, threshold = optCutOff)

# xây dựng ma trận nhầm lẫn 

confusionMatrix(test.set$y,Prediction, threshold = optCutOff)

lvs <- c("no", "yes")
truth <- factor(rep(lvs, times = c(9940, 1363)),
                levels = rev(lvs))
pred <- factor(
  c(
    rep(lvs, times = c(9596 , 344 )),
    rep(lvs, times = c(783, 580))),
  levels = rev(lvs))

caret::confusionMatrix(pred, truth)

table <- data.frame(caret::confusionMatrix(pred, truth)$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1,size = 10) + scale_fill_manual(values = c(good = "forestgreen", bad = "firebrick1")) +
  theme_bw() +xlim(rev(levels(table$Reference))) +theme(text = element_text(size=10)) +xlab("True Value")

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
