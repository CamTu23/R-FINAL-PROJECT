# Import library
library(tidyverse)
library(mlbench)
library(gmodels)
library(class)

# Load data
bank_train.path <- "./DATA/train.csv"
bank_test.path <- "./DATA/test.csv"

main_df <- read.table(bank_train.path,header = T, sep = ";")
test_df <- read.table(bank_test.path,header = T, sep = ";")
head(main_df)

str(main_df)
# Split train/test dataset
fact <- c(2:5,7:9,11,16,17)
num <- c(1,6,10,12:15)

for (i in fact)
{
  print(names(main_df)[i])
  print("Train Data(%):")
  print(round(prop.table(table(main_df[,i]))*100,1))
  print("Test Data(%):")
  print(round(prop.table(table(test_df[,i]))*100,1))
}

for (i in num)
{
  print(names(main_df)[i])
  print("Train Data(%):")   
  print(summary(main_df[,i]))
  print("Test Data(%):")
  print(summary(test_df[,i]))
}
# Kiểm tra null và NA
any(is.null(main_df))
any(is.na(main_df))

# Trực quan hóa dữ liệu
# Biến liên tục
# Biến rời rạc
y_bar <- ggplot(main_df, aes(y))
y_bar + geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=30))
# tạo hàm điều chỉnh figure size
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
# Create Logistic Regression model
# Tạo bản sao dữ liệu
lgr.df <- main_df
head(lgr.df)
# Tạo cột giả
library(fastDummies)
lgr.df = dummy_cols(lgr.df, select_columns = c("job","marital","education","contact","month","poutcome"))
head(lgr.df)

str(lgr.df)

lgr.df <- lgr.df[,-c(2,3,4,9,11,16,18,24,27,29,30,40,47)]
head(lgr.df)

lgr.df$default = as.numeric(lgr.df$default)
lgr.df$housing = as.numeric(lgr.df$housing)
lgr.df$loan = as.numeric(lgr.df$loan)
lgr.df$y = as.numeric(lgr.df$y)

lgr.df$default = ifelse(lgr.df$default == 2, 1, 0)
lgr.df$housing = ifelse(lgr.df$housing == 2, 1, 0)
lgr.df$loan = ifelse(lgr.df$loan == 2, 1, 0)
lgr.df$y = ifelse(lgr.df$y == 2, 1, 0)

str(lgr.df)
# Prediction

# Sigmoid