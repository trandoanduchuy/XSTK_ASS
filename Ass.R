# Download package (tai cac thu vien):
library("dplyr")      #analysize data
library("ggplot2")    #draw plot
library("readr")      #import data
library("tidyverse")  #draw boxplot

# I/ Nhập dữ liệu (import data):
setwd("D:/Education/Computer Engineering/Semester 221/Probability and Statistics/Assignment/Data")
data<-read.csv("auto_mpg.csv", header = TRUE, sep = ";",  na.strings=c("?"))

# II/ Làm sạch dữ liệu (cleanning data):
# 1) Trích ra tệp con bao gồm các biến chính
new_data = data.frame(mpg = data$mgp,
                      cylinders = data$cylinders,
                      displacement = data$displacement,
                      horsepower = data$horsepower,
                      weight = data$weight,
                      acceleration = data$acceleration,
                      model_year = data$model_year,
                      origin = data$origin)
# 1) Loại bỏ các dữ liệu bị khuyết
apply(is.na(new_data), 2, sum)  # thống kê số lượng quan sát khuyết đối với từng biến
apply(is.na(new_data), 2, mean) # thống kê tỷ lệ giá trị khuyết đối với từng biến
new_data <- na.omit(new_data)   # xóa các quan sát mà có giá trị khuyết

nrow(data)                               # số quan sát ban đầu  
nrow(new_data)                           # số quan sát còn lại sau khi xóa
nrow(data) - nrow(new_data)              # số lượng quan sát đã bị xóa
(nrow(data) - nrow(new_data))/nrow(data) # tỷ lệ các quan sát đã bị xóa

# III/ Data visualization (làm rõ dữ liệu):
# 1) Chuyển các biến định lượng sang dạng log:
new_data[,c("mpg","displacement","horsepower","weight","acceleration")] <- log(new_data[,c("mpg","displacement","horsepower","weight","acceleration")])
# 2) Tính các giá trị đặc trưng cho các biến định lượng
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

# 3) Vẽ biểu đồ phân phối (histogram) cho biến phụ thuộc:
mpg_his = ggplot(new_data, aes(mpg)) + 
  geom_histogram(col="white", aes(fill=..count..)) + 
  scale_fill_gradient("frequency", low="green", high="red") + 
  xlab("value of mpg") + 
  ylab("frequency") + 
  ggtitle('histogram of mpg')

# 4) xác định số lượng từng chủng loại cho các biến định tính
table_quanlitative <- apply(new_data[,c("cylinders", "model_year", "origin")], 2, table)

# 5) Vẽ biểu đồ cho biến phụ thuộc mpg
# a. Vẽ biểu đồ hộp (boxplot) cho biến mpg 
# theo các biến độc lập định tính cylinders, model_year, origin
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
# b. Vẽ biểu đồ phân tán (scatterplots) cho biến mpg 
# theo các biến độc lập định lượng displacement, horsepower, weight, acceleration
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

# Iv/ Chia bộ dữ liệu thành hai mẫu
sample_size = floor(0.512*nrow(new_data))
set.seed(777)

picked = sample(seq_len(nrow(new_data)), size = sample_size)
auto_mpg1 = new_data[picked,]      # mẫu huấn luyện
auto_mpg2 = new_data[-picked,]     # mẫu kiểm tra

# V/ Mô hình hồi quy tuyến tính
# 1) Xây dựng mô hình với forward selection
null = lm(mpg~1, data = auto_mpg1)
fw = step(null, mpg~cylinders+displacement+horsepower+weight+acceleration+model_year+origin, dierection = "forward", test = "F", k = log(nrow(auto_mpg1)))
summary(fw)

# 2) Xây dựng mô hình tuyến tính với backward elimination
full = lm(mpg~cylinders+displacement+horsepower+weight+acceleration+model_year+origin, data = auto_mpg1)
bw = step(full, direction = "backward", test = "F", k = log(nrow(auto_mpg1)))
summary(bw)
        
# 3) Xây dựng mô hình hồi quy với stepwise regression
null1 = lm(mpg~1,data = auto_mpg1)
bd = step(null1, mpg~cylinders+displacement+horsepower+weight+acceleration+model_year+origin, direction = "both", test = "F", k = log(nrow(auto_mpg1)))
bd$anova
summary(bd)