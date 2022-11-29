# Download package (tai cac thu vien):
library("dplyr")      #analysize data
library("ggplot2")    #draw plot
library("readr")      #import data
library("tidyverse")  #draw boxplot

# I/ import data (nhap du lieu):
setwd("D:/Education/Computer Engineering/Semester 221/Probability and Statistics/Assignment/Data")
data<-read.csv("auto_mpg.csv", header = TRUE, sep = ";",  na.strings=c("?"))

# II/ Làm sạch dữ liệu (cleanning data):
# 1) Loại bỏ các dữ liệu bị khuyết
apply(is.na(data), 2, sum)  # thống kê số lượng quan sát khuyết đối với từng biến
apply(is.na(data), 2, mean) # thống kê tỷ lệ giá trị khuyết đối với từng biến
new_data <- na.omit(data)   # xóa các quan sát mà có giá trị khuyết

nrow(data)                               # số quan sát ban đầu  
nrow(new_data)                           # số quan sát còn lại sau khi xóa
nrow(data) - nrow(new_data)              # số lượng quan sát đã bị xóa
(nrow(data) - nrow(new_data))/nrow(data) # tỷ lệ các quan sát đã bị xóa
# 2) đặt lại tên và loại bỏ bớt biến
new_data = data.frame(mpg = new_data$mgp,
                     cylinders = new_data$cylinders,
                     displacement = new_data$displacement,
                     horsepower = new_data$horsepower,
                     weight = new_data$weight,
                     acceleration = new_data$acceleration,
                     model_year = new_data$model_year,
                     origin = new_data$origin)

# III/ Data visualization (làm rõ dữ liệu):
# 1) Tính các giá trị đặc trưng cho các biến định lượng
mpg <- new_data$mpg
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
table_quantitatve <- t(data.frame(length, mean, sd, min, max, Q1, Q2, Q3)) # tạo bảng thống kê

# 2) xác định số lượng từng chủng loại cho các biến định tính
table_quanlitative <- apply(new_data[,c("cylinders", "model_year", "origin")], 2, table)

# 3) Vẽ biểu đồ phân phối (histogram) cho biến phụ thuộc:
mpg_his = ggplot(new_data, aes(mpg)) + 
  geom_histogram(col="white", aes(fill=..count..)) + 
  scale_fill_gradient("frequency", low="green", high="red") + 
  xlab("miles/gallon") + 
  ylab("frequency") + 
  ggtitle('histogram of mpg')

# 4) Vẽ biểu đồ cho biến phụ thuộc mpg
# a. Vẽ biểu đồ hộp (boxplot) cho biến mpg theo cylinders, model_year, origin
cylinders_boxplot = boxplot(mpg~cylinders, 
                  data = new_data, 
                  xlab = "Number of cylinders",
                  ylab = "Miles Per Gallon",
                  main = "box plot of mpg by cylinders",
                  col = 2:6)
modelyear_boxplot = boxplot(mpg~model_year, 
                 data = new_data, 
                 xlab = "model year",
                 ylab = "Miles Per Gallon",
                 main = "box plot of mpg by model year",
                 col = 2:6)
origin_boxplot = boxplot(mpg~origin, 
                        data = new_data,
                        xlab = "Origin",
                        ylab = "Miles Per Gallon",
                        main = "box plot of mpg by origin",
                        col = 2:6)
# b. Vẽ biểu đồ phân tán (scatterplots) cho biến mpg theo displacement, horsepower, weight, acceleration
displacement_scatterplot = pairs(mpg~displacement, 
                                 data = new_data,
                                 labels = c("mpg", "displacement"),
                                 col = "#2d3166", 
                                 main = "scatter plot of mpg by displacement")

horsepower_scatterplot = pairs(mpg~horsepower, 
                                 data = new_data,
                                 labels = c("mpg", "horsepower"),
                                 col = "#2d3166", 
                                 main = "scatter plot of mpg by horsepower")
weight_scatterplot = pairs(mpg~weight, 
                               data = new_data,
                               labels = c("mpg", "weight"),
                               col = "#2d3166", 
                               main = "scatter plot of mpg by weight")
acceleration_scatterplot = pairs(mpg~acceleration, 
                           data = new_data,
                           labels = c("mpg", "acceleration"),
                           col = "#2d3166", 
                           main = "scatter plot of mpg by acceleration")

