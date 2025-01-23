
data <-read.csv("C:/Users/RABBI/Desktop/data/panic_disorder_dataset_training.csv/panic_disorder_dataset_training.csv", header = TRUE , sep = ",")
print(data)
names(data)
summary(data)
sum(is.na(data))

#covert numerical to categorical 
mydata<-data
mydata$Panic.Disorder.Diagnosis <- factor(mydata$Panic.Disorder.Diagnosis, levels = c(0,1),labels = c("Negative","Positive"))
print(mydata$Panic.Disorder.Diagnosis)

mydata$Age <- ifelse(mydata$Age <= 18, "teen",
                     ifelse(mydata$Age <=45, "Adults",
                            ifelse(mydata$Age <=100, "Old")))

print(mydata$Age)
print(mydata)
names(mydata)

#correlation ---------------------------
library(MASS) 

ag <- data.frame(mydata$Age, mydata$Panic.Disorder.Diagnosis)
ag = table(mydata$Age, mydata$Panic.Disorder.Diagnosis)
print(ag)
print(chisq.test(ag))  #find age corelation p-value  is 0.78 which is  greater then 0.05 

chi_data <- data.frame(mydata$Gender,mydata$Panic.Disorder.Diagnosis)
chi_data = table(mydata$Gender,mydata$Panic.Disorder.Diagnosis)
print (chi_data)
print(chisq.test(chi_data))  #this also have p-value greater then 0.05


family <- data.frame(data$Family.History,mydata$Panic.Disorder.Diagnosis)
family = table(mydata$Family.History,mydata$Panic.Disorder.Diagnosis)
print(family)
print(chisq.test(family))


person <- data.frame(mydata$Personal.History,mydata$Panic.Disorder.Diagnosis)
person = table(mydata$Personal.History,mydata$Panic.Disorder.Diagnosis)
print(person)
print(chisq.test(person))


stressor <- data.frame(mydata$Current.Stressors,mydata$Panic.Disorder.Diagnosis)
stressor = table(mydata$Current.Stressors,mydata$Panic.Disorder.Diagnosis)
print(stressor)
print(chisq.test(stressor))

symptoms <- data.frame(mydata$Symptoms, mydata$Panic.Disorder.Diagnosis)
symptoms = table(mydata$Symptoms,mydata$Panic.Disorder.Diagnosis)
print(symptoms)
print(chisq.test(symptoms))

severity <- data.frame(mydata$Severity,mydata$Panic.Disorder.Diagnosis)
severity = table(mydata$Severity,mydata$Panic.Disorder.Diagnosis)
print(severity)
print(chisq.test(severity))

impact <- data.frame(mydata$Impact.on.Life, mydata$Panic.Disorder.Diagnosis)
impact = table(mydata$Impact.on.Life,mydata$Panic.Disorder.Diagnosis)
print(impact)
print(chisq.test(impact))

demographic <- data.frame(mydata$Demographics,mydata$Panic.Disorder.Diagnosis)
demographic = table(mydata$Demographics,mydata$Panic.Disorder.Diagnosis)
print(demographic)
print(chisq.test(demographic))

medical <- data.frame(mydata$Medical.History, mydata$Panic.Disorder.Diagnosis)
medical = table(mydata$Medical.History, mydata$Panic.Disorder.Diagnosis)
print(medical)
print(chisq.test(medical))

psychiatric <- data.frame(mydata$Psychiatric.History,mydata$Panic.Disorder.Diagnosis)
psychiatric = table (mydata$Psychiatric.History, mydata$Panic.Disorder.Diagnosis)
print(psychiatric)
print(chisq.test(psychiatric))

substance <- data.frame(mydata$Substance.Use,mydata$Panic.Disorder.Diagnosis)
substance =  table(mydata$Substance.Use, mydata$Panic.Disorder.Diagnosis)
print(substance)
print(chisq.test(substance))

coping <- data.frame(mydata$Coping.Mechanisms, mydata$Panic.Disorder.Diagnosis)
coping = table(mydata$Coping.Mechanisms, mydata$Panic.Disorder.Diagnosis)
print(coping)
print(chisq.test(coping))

social <- data.frame(mydata$Social.Support, mydata$Panic.Disorder.Diagnosis)
social = table(mydata$Social.Support, mydata$Panic.Disorder.Diagnosis)
print(social)
print(chisq.test(social))

lifestyle <- data.frame(mydata$Lifestyle.Factors, mydata$Panic.Disorder.Diagnosis)
lifestyle = table(mydata$Lifestyle.Factors, mydata$Panic.Disorder.Diagnosis)
print(lifestyle)
print(chisq.test(lifestyle))


#significant attributes in dataset for naive baye------------------------------------------------------------------

mydata <- subset(mydata,select = -c(Participant.ID,Age,Gender))
names(mydata)

#----------------------------------------naive baye-----------------------------------------------

# Install and load the required package
install.packages("e1071")
library(e1071)
your_data <-mydata

# Assuming you have a dataset named 'your_data' with features and a target variable
# Replace 'your_data' with the actual name of your dataset and modify features/target accordingly

# Split the dataset into training and testing sets
set.seed(123)
sample_index <- sample(1:nrow(your_data), nrow(your_data) * 0.7)
train_data <- your_data[sample_index, ]
test_data <- your_data[-sample_index, ]

# Train a Naive Bayes classifier
naive_bayes_model <- naiveBayes(Panic.Disorder.Diagnosis ~  Family.History + Personal.History + Current.Stressors + Symptoms + Severity + Impact.on.Life + Demographics
                                + Medical.History + Psychiatric.History + Substance.Use + Coping.Mechanisms+
                                  Social.Support+ Lifestyle.Factors, data = train_data)

# Make predictions on the test set
predictions <- predict(naive_bayes_model, newdata = test_data)

# Confusion matrix
conf_matrix <- table(predictions, test_data$Panic.Disorder.Diagnosis)
cat("Confusion Matrix:\n", conf_matrix, "\n")

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# 10 fold cross validation ----------------------------------------------
  

# Install and load the required packages
install.packages(c("e1071", "caret"))
library(e1071)
library(caret)

# Assuming you have a dataset named 'your_data' with features and a target variable
# Replace 'your_data' with the actual name of your dataset and modify features/target accordingly

# Create a control object for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train a Naive Bayes classifier with cross-validation
naive_bayes_model_cv <- train(Panic.Disorder.Diagnosis ~., data = your_data, method = "naive_bayes", trControl = ctrl)

# Access the cross-validation results
cv_results <- naive_bayes_model_cv$results
print("Cross-validation Results:")
print(cv_results)

# Calculate accuracy from cross-validation results
cv_accuracy <- mean(cv_results$Accuracy)
cat("Cross-validation Accuracy:", cv_accuracy, "\n")


#------------------------------------------------------confusion matrix--------------------
  # Calculate recall, precision, and F-measure
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f_measure <- 2 * (precision * recall) / (precision + recall)

cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F-measure:", f_measure, "\n")
