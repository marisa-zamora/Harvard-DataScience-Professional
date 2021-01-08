# Diabetes Project
# Author: Marisa Ivonne Zamora Carrillo
# January 2021


if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "https://cran.r-project.org/src/contrib/Archive/ROCR/ROCR_1.0-7.tar.gz")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(caret)
library(ROCR)
library(dplyr)
library(formattable)
library(naivebayes)
library(randomForest)

# Import the data --------------------------------

# https://archive.ics.uci.edu/ml/machine-learning-databases/00529/diabetes_data_upload.csv


data <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/00529/diabetes_data_upload.csv"
  )


# Data Analysis ---------------------------------
nrow(data)
data2 <- data %>% unique()
nrow(data2) #it appears to be some duplicated data, but since there are no person ID, just attributes we are going to assume there are no duplicate values, just different people with same attributes
str(data)%>% formattable()
#people with diabetes
data %>% ggplot(aes(x = class, fill = class)) + geom_bar() +
  labs(title = "Positive and Negative diagnosis", x = "Diasgnosis",
       y = "People")

#people grouped by age
data %>% ggplot(aes(x = Age, fill = Age)) + geom_bar() +
  labs(title = "Age", x = "Age",
       y = "People")

#people grouped by age and diagnosis
data %>% ggplot(aes(x = Age, fill = class)) + geom_bar() +
  labs(title = "Positive and Negative diagnosis", x = "Age",
       y = "People")

#people grouped by gender
data %>% ggplot(aes(x = Gender, fill = class)) + geom_bar() +
  labs(title = "Gender", x = "Gender",
       y = "People")


# Create the train and test datasets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data$class, times = 1, p = 0.1, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
train <- data.frame(train)
test <- data.frame(test)

# Predictive model building and evaluation -------------------------------------

# Create a dataframe to keep the results
results <- data.frame(model=character(0),accuracy=numeric(),sensitivity=numeric(),specificity=numeric())

#create a cross validation fit control with 3 folds

fit_control <- caret::trainControl(
  method = "cv",
  number = 3)

#logistic regresion ***

LR <- train(
  class ~ .,
  data = train,
  method = "regLogistic",
  trControl = fit_control
)   

predict_LR <- predict(LR,test)

LR_accuracy <- confusionMatrix(predict_LR,as.factor(test$class))$overall[["Accuracy"]]
LR_sensitivity <- confusionMatrix(predict_LR,as.factor(test$class))$byClass["Sensitivity"]
LR_specificity <- confusionMatrix(predict_LR,as.factor(test$class))$byClass["Specificity"]
results <- results %>% add_row(model="Logistic Regression", accuracy=LR_accuracy,sensitivity = LR_sensitivity,specificity=LR_specificity)

#naive bayes model
naive_model <- naive_bayes(class ~ ., data = train, laplace=1)

predict_NB <- predict(naive_model, newdata=test)

NB_accuracy <- confusionMatrix(predict_NB,as.factor(test$class))$overall[["Accuracy"]]
NB_sensitivity <- confusionMatrix(predict_NB,as.factor(test$class))$byClass["Sensitivity"]
NB_specificity <- confusionMatrix(predict_NB,as.factor(test$class))$byClass["Specificity"]
results <- results %>% add_row(model="Naive Bayes", accuracy=NB_accuracy,sensitivity = NB_sensitivity,specificity=NB_specificity)

#Random Forest
train$class = factor(train$class)
RF <- randomForest(class~., data = train)

predict_RF <- predict(RF, newdata=test)
RF_accuracy <- confusionMatrix(predict_RF,as.factor(test$class))$overall[["Accuracy"]]
RF_sensitivity <- confusionMatrix(predict_RF,as.factor(test$class))$byClass["Sensitivity"]
RF_specificity <- confusionMatrix(predict_RF,as.factor(test$class))$byClass["Specificity"]
results <- results %>% add_row(model="Random Forest", accuracy=RF_accuracy,sensitivity = RF_sensitivity,specificity=RF_specificity)


# Results -------------------------------------------------------------------
results%>%formattable()
results %>% ggplot() + geom_point(aes(model, accuracy,shape=model,size=1, color = "accuracy")) +
  geom_point(aes(model, sensitivity,shape=model,size=1, color = "sensitivity")) +
  geom_point(aes(model, specificity, shape=model,size=1,color = "specificity")) +
  labs(title = "Model Results",
       y = ". ",
       x = "Model")+
  guides(shape = FALSE, size = FALSE)+theme(legend.title = element_blank())
  
