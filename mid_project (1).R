
data <-read.csv("C:/Users/RABBI/Desktop/data set/dt.csv", header = TRUE , sep = ",")
print(data)
names(data)
 
hist(data$age)
hist(data$impluse)
hist(data$pressurehight)
hist(data$pressurelow)
hist(data$glucose)
barplot(data$gender)

install.packages("ggplot2")
library(ggplot2)
gender_data <- data.frame( Gender=data$gender)
ggplot(gender_data, aes(x = Gender)) +
  geom_bar(fill = "yellow") +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count")

class_data <- data.frame( Class=data$class)
ggplot(class_data, aes(x = Class)) +
  geom_bar(fill = "green") +
  labs(title = " Positive and Negative Value",
       x = "class",
       y = "Count")


data_completeness <- colMeans(is.na(data))
print(data_completeness)

mydata <-data
mydata$gender <- factor(mydata$gender, levels=c("male","female"),labels =c(1,2))
mydata$class <- factor(mydata$class, levels = c("positive","negative"),labels = c(1,2))
print(mydata)


print(paste("Missing value in Age column " , Sum <- sum(is.na(mydata$age))))
print(paste("Missing value in gender column " , Sum <- sum(is.na(mydata$gender))))
print(paste("Missing value in impluse column " , Sum <- sum(is.na(mydata$impluse))))
print(paste("Missing value in pressurehight column " , Sum <- sum(is.na(mydata$pressurehight))))
print(paste("Missing value in pressurelow column " , Sum <- sum(is.na(mydata$pressurelow))))
print(paste("Missing value in glucose column " , Sum <- sum(is.na(mydata$glucose))))

null_ds <- mydata
rm_nll <- na.omit(mydata)
summary(rm_nll)


mean_ds <- mydata
mean_ds$age[is.na(mean_ds$age)] <-mean(mydata$age, na.rm = TRUE)
mean_ds$pressurehight[is.na(mean_ds$pressurehight)] <-mean(mydata$pressurehight, na.rm = TRUE)
summary(mean_ds)

 
median_ds <-mydata
median_ds$age[is.na(median_ds$age)] <-median(mydata$age, na.rm = TRUE)
median_ds$pressurehight[is.na(median_ds$pressurehight)] <-median(mydata$pressurehight, na.rm = TRUE)
summary(median_ds)



boxplot(mydata$age,main="age")
boxplot.stats(mydata$age)$out

boxplot(mydata$impluse,main="Impluse")
boxplot.stats(mydata$impluse)$out

boxplot(mydata$pressurehight,main="pressurehigh")
boxplot.stats(mydata$pressurehight)$out

boxplot(mydata$pressurelow,main="pressurelow")
boxplot.stats(mydata$pressurelow)$out