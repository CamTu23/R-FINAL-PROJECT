# Phân tích khách hang nào có tiền gửi va đặt tiên gửi 
library(tidyverse)
library(mlbench)
library(gmodels)
library(class)

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

# Gán 
lr.df <- Mt_df
# Đọc n dòng đầu 
# head(x,n=number), x = input dataset / dataframe, n = number of rows that the function should display.
head(lr.df)

#Tạo cột giả, hàng giả
library(fastDummies)
# Những cột giá trị string 
# Tạo cột giả cho các thuộc tính trong bảng
lr.df = dummy_cols(lr.df, select_columns = c("job","marital","education","contact","month","poutcome"))
head(lr.df)

# Xóa các cột trong bảng
lr.df <- lr.df[,-c(2,3,4,9,11,16,18,24,27,29,30,40,47)]
head(lr.df)
str(lr.df)

# Chuyển đổi dữ liệu từ yes/no về 1/0
lr.df$default = ifelse(lr.df$default == "yes",1,0)
lr.df$housing = ifelse(lr.df$housing == "yes",1,0)
lr.df$loan = ifelse(lr.df$loan == "yes",1,0)
lr.df$y = ifelse(lr.df$y=="yes",1,0)
lr.df
str(lr.df)

#library(mccr)
# 1. Chẩn đoán điểm xác suất dự đoán (Diagnostics of predicted probability scores)
# 2. Phân tích hiệu suất (Performance analysis)
# 3. Các chức năng hỗ trợ cải thiện độ chính xác (Functions that aid accuracy improvement)
library(InformationValue)
# sử dụng cùng một set.seed trong cả ba mẫu sample nên chúng tôi thu được các giá trị mẫu giống nhau.
# Tạo sample k có set.seed thì các mẫu thu được khác nhau
# sử dụng set.seed là để đảm bảo rằng chúng ta nhận được cùng một kết quả cho việc ngẫu nhiên hóa.
# xây dựng mô hình trên tập huấn luyện và đánh giá hiệu suất của nó trên tập thử nghiệm 
# => phương pháp xác nhận lưu giữ để đánh giá hiệu suất của mô hình.
# Phân vùng dữ liệu
set.seed(8)
# Làm tròn floor (kích thước/độ dài của tập train của tập lr.df)
train.size = floor(0.75*nrow(lr.df))
# Tạo chỉ mục cho các quan sát lấy mẫu ngẫu nhiên để phân vùng dữ liệu, lấy ngẫu nhiên 0,75*n dòng của tập lr.df
train.index = sample(1:nrow(lr.df), train.size)
# Tạo tập huấn luyện và kiểm tra
# Tập huấn luyện chứa 75% dữ liệu
train.set = lr.df[train.index,]
# Tập test chứa 25% còn lại
test.set = lr.df[-train.index,]

# Tính logit
# link = "logit" là một đặc tả cho chức năng liên kết mô hình. Đây có thể là một tên / biểu thức, một chuỗi ký tự chữ, một vectơ độ dài một ký tự hoặc một đối tượng của lớp.
# binomial: Hồi quy logistic nhị phân, hữu ích khi phản hồi là 0 hoặc 1.
# logit mặc định cho binomial
# sử dụng hàm glm() để hồi qui các model theo phương pháp hồi qui tổng quát.
# gl(formula, data, family): Formula –  Trình bày mối quan hệ giữa các biến. Data là tập dữ liệu đưa ra các giá trị của các biến này. Các family là đối tượng nghiên cứu để xác định các chi tiết của mô hình. Giá trị của nó là nhị thức đối với Logistic Regression.
logit_reg = glm(y ~ ., data = train.set, family = binomial(link = "logit"))
summary(logit_reg)

# Dự báo: mô hinh nhị thức thì dự đoán mặc định là log-odds và type = "response" (0 hoặc 1) cho tỷ lệ dự đoán 
Prediction = predict(logit_reg, test.set, type = "response")
plotROC(test.set$y, Prediction)

# Ma trận hỗn loạn, Đồ thị trực quan của 2 yếu tố Thực tế và Dự đoán => đo lường hiệu suất của mô hình Classification
# Mặc định ngưỡng (threshold) được sử dụng là 0.5, tức là 1 điểm dữ liệu x sẽ được dự đoán rơi vào lớp 1 nếu giá trị predict_proba (x) > 0.5 và ngược lại

confusionMatrix(test.set$y,Prediction, threshold = 0.5)
# https://baoninhsunrise.com/confusion-matrix-la-gi/

# 
library(caret)

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

optCutOff = optimalCutoff(test.set$y, Prediction, optimiseFor = "misclasserror")
optCutOff


confusionMatrix(test.set$y,Prediction, threshold = optCutOff)

# data/code from "2 class example" example courtesy of ?caret::confusionMatrix

lvs <- c("no", "yes")
truth <- factor(rep(lvs, times = c(9940, 1363)),
                levels = rev(lvs))
pred <- factor(
  c(
    rep(lvs, times = c(9562, 378)),
    rep(lvs, times = c(740, 623))),
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
