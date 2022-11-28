# I/ Download package (tai cac thu vien):
library("dplyr")      #analysize data
library("ggplot2")    #draw plot
library("readr")      #import data
library("tidyverse")  #draw boxplot
# I/ import data (nhap du lieu):
setwd("D:/Education/Computer Engineering/Semester 221/Probability and Statistics/Assignment/Data")
data<-read.csv("auto_mpg.csv", header = TRUE, sep = ";",  na.strings=c("?"))

# II/ Làm sạch dữ liệu (cleanning data):
# 1) exclude missing data
apply(is.na(data), 2, sum)  # thống kê số lượng quan sát khuyết đối với từng biến
apply(is.na(data), 2, mean) # thống kê tỷ lệ giá trị khuyết đối với từng biến
new_data <- na.omit(data)   # xóa các quan sát mà có giá trị khuyết

nrow(data)                               # số quan sát ban đầu  
nrow(new_data)                           # số quan sát còn lại sau khi xóa
nrow(data) - nrow(new_data)              # số lượng quan sát đã bị xóa
(nrow(data) - nrow(new_data))/nrow(data) # tỷ lệ các quan sát đã bị xóa

# III/ Data visualization (làm rõ dữ liệu):
# 1) xử lý biến định lượng liên tục
# a. Tính các giá trị đặc trưng
mpg <- new_data$mgp
displacement <- new_data$displacement
horsepower <- new_data$horsepower
weight <- new_data$weight
acceleration <- new_data$acceleration
convariables <- data.frame(mpg, displacement, horsepower, weight, acceleration)

length = apply(convariables, 2, length) # tính kích thước
mean = apply(convariables, 2, mean) # tính kỳ vọng
sd = apply(convariables, 2, sd) # tính độ lệch chuẩn
min = apply(convariables, 2, min) # tính giá trị nhỏ nhất
max = apply(convariables, 2, max) # tính giá trị lớn nhất
Q1 = apply(convariables, 2, quantile, probs = 0.25) # tính phân vị 1
Q2 = apply(convariables, 2, quantile, probs = 0.5) # tính phân vị 2
Q3 = apply(convariables, 2, quantile, probs = 0.75) # tính phân vị 3
t(data.frame(length, mean, sd, min, max, Q1, Q2, Q3)) # tạo bảng thống kê

# b. Vẽ biểu đồ hộp (boxplot):
mpg_bp = boxplot(mpg ~ cylinders, 
                 data = new_data, 
                 xlab = "Number of Cylinders",
                 ylab = "Miles Per Gallon",
                 main = "boxplot of mpg grouped by cylinders",
                 col = 2:6)
mpg_bp = boxplot(displacement ~ cylinders, 
                 data = new_data, 
                 xlab = "Number of Cylinders",
                 ylab = "displacement",
                 main = "boxplot of displacement grouped by cylinders",
                 col = 2:6)
# c. Xử lý các giá trị ngoại lai:

# d. Vẽ biểu đồ tần suất (histogram:
mpg = ggplot(convariables, aes(mpg)) + 
  geom_histogram(col="white", aes(fill=..count..)) + 
  scale_fill_gradient("frequency", low="green", high="red") + 
  xlab("miles/gallon") + 
  ylab("frequency") + 
  ggtitle('histogram of continuous variable mpg')


