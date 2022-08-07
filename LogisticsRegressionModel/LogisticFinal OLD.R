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
library(caTools)
library(epiDisplay)
library(DescTools)
library(rms)
library(ROCR)
library(InformationValue)
library(glmnet)


library(readr)
f <- file.choose() # Chọn file DL.csv trong thư mục DATA của mục Logistic Model
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

ggplot(train.dl, aes(default))+geom_bar(color = "black",fill = "khaki") + theme(text = element_text(size=10))

ggplot(train.dl, aes(housing)) + geom_bar(color = "black",fill = "firebrick2") + theme(text = element_text(size=10))
round(prop.table(table(train.dl$housing))*100,1)

ggplot(train.dl, aes(loan)) + geom_bar(color = "black",fill = "darkviolet") + theme(text = element_text(size=10))
round(prop.table(table(train.dl$loan))*100,1)


#kt phan phoi bien lien tuc
fig(12, 8)
#ggplot(train.dl, aes(age,color = y)) + geom_histogram(binwidth = 5, color = "black",fill = "green") + theme(text = element_text(size=10))
ggplot(data = train.dl, aes(age, color = y))+ geom_freqpoly(binwidth = 5, size = 1)
summary(train.dl$age)
fig(12, 8)
#ggplot(train.dl, aes(balance)) + geom_area(stat = "bin", color = "black",fill = "cyan2",alpha = 0.5) + theme(text = element_text(size=10))
ggplot(data = train.dl, aes(balance, color = y))+ geom_freqpoly(binwidth = 1000, size = 1)
summary(train.dl$balance)
fig(20, 8)
#ggplot(train.dl, aes(duration)) + geom_area(stat = "bin", color = "black",fill = "pink2", alpha = 0.5) + theme(text = element_text(size=10))
ggplot(data = train.dl, aes(duration, color = y))+ geom_freqpoly(binwidth = 100, size = 1)
summary(train.dl$duration)
fig(20, 8)
ggplot(data = train.dl, aes(campaign, color = y))+ geom_freqpoly(binwidth = 5, size = 1)
summary(train.dl$campaign)
fig(20, 8)
ggplot(data = train.dl, aes(day, color = y))+ geom_freqpoly(binwidth = 5, size = 1)
summary(train.dl$day)
fig(20, 8)
ggplot(data = train.dl, aes(pdays, color = y))+ geom_freqpoly(binwidth =100, size = 1)
summary(train.dl$pdays)
fig(20, 8)
ggplot(data = train.dl, aes(previous, color = y))+ geom_freqpoly(binwidth = 100, size = 1)
summary(train.dl$previous)


# fig(20, 8)
# ggplot(train.dl, aes(day)) + geom_area(stat = "bin", color = "black",fill = "slateblue1", alpha = 0.5) + theme(text = element_text(size=10))
# summary(train.dl$day)
# fig(20, 8)
# ggplot(train.dl, aes(pdays)) + geom_area(binwidth = 10, stat = "bin" ,alpha = 0.5, color = "black",fill = "deepskyblue3") + theme(text = element_text(size=10))
# summary(train.dl$pdays)
# fig(20, 8)
# ggplot(train.dl, aes(previous))+ geom_area(binwidth = 10, stat = "bin" ,alpha = 0.5, color = "black",fill = "brown") + theme(text = element_text(size=10))
# summary(train.dl$previous)

train.dl %>%
  dplyr::select (age,balance,duration,campaign,day,pdays,previous) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)


#### Categorical Variables
table(train.dl[("job")])
table(train.dl[("marital")])
table(train.dl[("education")])
table(train.dl[("poutcome")])
table(train.dl[('contact')])
table(train.dl[('month')])

fig(16, 8)
ggplot(train.dl, aes(job)) + geom_bar(aes(x = job, fill = y)) +  theme(text = element_text(size=10), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
round(prop.table(table(train.dl$job))*100,1)
fig(16, 8)
ggplot(train.dl, aes(marital)) + geom_bar(aes(x = marital, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$marital))*100,1)
fig(16, 8)
ggplot(train.dl, aes(education))+ geom_bar(aes(x = education, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$education))*100,1)

ggplot(train.dl, aes(poutcome)) + geom_bar(aes(x = poutcome, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$poutcome))*100,1)
ggplot(train.dl, aes(contact)) + geom_bar(aes(x = contact, fill = y)) + theme(text = element_text(size=10))
round(prop.table(table(train.dl$contact))*100,1)




###Logistic 
###Processing
lr.df <- train.dl
head(lr.df)
summary(lr.df)
##tạo các cột giả (nhị phân) từ các cột kiểu ký tự
library(fastDummies)

lr.df = dummy_cols(lr.df, select_columns = c("job","marital","education","contact","month","poutcome"))
head(lr.df)
#options(max.print=5000)
str(lr.df)

#Xoa cot chu Dl 
lr.df <- lr.df[,-c(2,3,4,9,11,16,18,24,27,29,30,40,47)]
head(lr.df)

#đổi DL
lr.df$default=ifelse(lr.df$default=='yes',1,0)
lr.df$housing=ifelse(lr.df$housing=='yes',1,0)
lr.df$loan=ifelse(lr.df$loan=='yes',1,0)
lr.df$y = ifelse(lr.df$y == 'yes', 1, 0)
str(lr.df)

#chia bo mau 
library(caTools)
library(InformationValue)
set.seed(8)
#assignment <- sample(0:1, size= nrow(lr.df), prob = c(0.75,0.25), replace = TRUE)
#train.set <- lr.df[assignment == 0, ]
#test.set <- lr.df[assignment == 1, ]

#split=sample.split(lr.df$y,SplitRatio=0.75)
#train.set=subset(lr.df,split==TRUE)
#test.set=subset(lr.df,split==FALSE)

train.size = floor(0.75*nrow(lr.df))
train.index = sample(1:nrow(lr.df), train.size)
train.set = lr.df[train.index,]
test.set = lr.df[-train.index,]

##library(dplyr)
##library(magrittr)
library(dplyr)
library(magrittr)
train.set %>%
  summarise(Total = n())
test.set %>%
  summarise(Total = n())


####Build Logit Models and Predict 
#mô hình hồi quy Logistic trên tất cả các biến 

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
library(InformationValue)
optCutOff <- optimalCutoff(test.set$y, Prediction)[1] 
print(optCutOff)

##Model Diagnostics
#tính toán tổng tỷ lệ lỗi phân loại sai
misClassError(test.set$y, Prediction, threshold = optCutOff)

#ROC 
library(ROCR)
library(InformationValue)
#vẽ ROC
plotROC(test.set$y, Prediction)


#library(GGally)
#ggcoef(logit_reg,exponentiate=T,exclude_intercept=T,vline_color="red",errorbar_color="blue",errorbar_height=0.10)
#plot(ggcoef)

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
    rep(lvs, times = c(9591, 349)),
    rep(lvs, times = c(774, 589))),
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




