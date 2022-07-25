# Phân tích khách hàng nào có tiền gửi và đặt tiền gửi 
library(tidyverse)
library(mlbench)
library(gmodels)
library(class)
library(fastDummies)
library(InformationValue)
library(caret)

# Gán đường dẫn cho train.path, test.path
train.path <- "./DATA/train.csv"
test.path <- "./DATA/test.csv"

# Đọc dữ liệu từ file gán cho Mt_df, test_df
Mt_df <- read.table(train.path,header = T, sep = ";")
test_df <- read.table(test.path,header = T, sep = ";")

# Đọc n row đầu của dataset
head(Mt_df)

# Chuyển dữ liệu về dạng string
str(Mt_df)

# Preprocessing
# Creating a copy of the dataset. 
lr.df <- Mt_df
head(lr.df)

# Tạo các cột giả cho các thuộc tính trong bảng
lr.df = dummy_cols(lr.df, select_columns = c("job","marital","education","contact","month","poutcome"))
head(lr.df)

#Clean data
# Xóa các cột trong bảng
lr.df <- lr.df[,-c(2,3,4,9,11,16,18,24,27,29,30,40,47)]
head(lr.df)
str(lr.df)

# Convert data
# Chuyển đổi dữ liệu từ yes/no về 1/0
lr.df$default = ifelse(lr.df$default == "yes",1,0)
lr.df$housing = ifelse(lr.df$housing == "yes",1,0)
lr.df$loan = ifelse(lr.df$loan == "yes",1,0)
lr.df$y = ifelse(lr.df$y=="yes",1,0)
lr.df
str(lr.df)

# Split data
# Sử dụng set.seed => cùng một kết quả cho việc lấy mẫu ngẫu nhiên 
set.seed(8)
train.size = floor(0.75*nrow(lr.df))
# Tạo chỉ mục cho các quan sát lấy mẫu ngẫu nhiên để phân vùng dữ liệu
train.index = sample(1:nrow(lr.df), train.size)
# Tạo tập huấn luyện và kiểm tra
# Tập huấn luyện chứa 75% dữ liệu
train.set = lr.df[train.index,]
# Tập test chứa 25% còn lại
test.set = lr.df[-train.index,]

# Creating model
# Sử dụng hàm glm() để hồi qui các model theo phương pháp hồi qui tổng quát
# glm(formula, data, family): Formula: Trình bày mối quan hệ giữa các biến, binomial: Hồi quy logistic nhị phân, response là 0 hoặc 1. Logit mặc định cho binomial 
logit_reg = glm(y ~ ., data = train.set, family = binomial(link = "logit"))
# tóm tắt thống kê về dữ liệu
summary(logit_reg)

# Predict model
# Dự báo: mô hình nhị thức thì dự đoán mặc định là log-odds và type = "response" (0 hoặc 1) cho tỷ lệ dự đoán 
Prediction = predict(logit_reg, test.set, type = "response")
# Vẽ biểu đồ
plotROC(test.set$y, Prediction)
# => Mô hình trên có diện tích đường cong ROC = 90,23% => Khả năng dự đoán của mô hình rất tốt.

# Concordance (Sự phù hợp)
Concordance(test.set$y, Prediction)
# => Mô hình trên với tỷ lệ 90,5% => Một mô hình có chất lượng tốt.

# Độ đặc hiệu và độ nhạy (Specificity and Sensitivity)
specificity(test.set$y, Prediction, threshold = optCutOff) #False Positive Rate
# => 96,5%
sensitivity(test.set$y, Prediction, threshold = optCutOff) #True Positive Rate
# => 43,2%
# => Các con số trên được tính toán trên mẫu test không được sử dụng để training. Vì vậy, tỷ lệ phát hiện sự thật là 43,2% trên dữ liệu thử nghiệm là tốt.

# Xây dựng ma trận hỗn loạn Confusion matrix
# Ma trận hỗn loạn, Đồ thị trực quan của 2 yếu tố Thực tế và Dự đoán => đo lường hiệu suất của mô hình Classification
# Mặc định ngưỡng (threshold) được sử dụng là 0.5, tức là 1 điểm dữ liệu x sẽ được dự đoán rơi vào lớp 1 nếu giá trị predict_proba (x) > 0.5 và ngược lại
confusionMatrix(test.set$y,Prediction, threshold = 0.5)

# data/code from "2 class example" example courtesy of ?caret::confusionMatrix

lvs <- c("no", "yes")
truth <- factor(rep(lvs, times = c(9940, 1363)),
                levels = rev(lvs))
pred <- factor(
  c(
    rep(lvs, times = c(9712, 228)),
    rep(lvs, times = c(901, 462))),
  levels = rev(lvs))

caret::confusionMatrix(pred, truth)

table <- data.frame(caret::confusionMatrix(pred, truth)$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1,size = 30) +
  scale_fill_manual(values = c(good = "forestgreen", bad = "firebrick1")) +
  theme_bw() +
  xlim(rev(levels(table$Reference))) +
  theme(text = element_text(size=30)) +
  xlab("True Value")

# => Accuracy: 90% => mô hình tốt
