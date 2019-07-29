library(e1071)
library(caret)

# Data Input

data <- read.csv("C:/Users/Magilan/Desktop/ML_project/austin_weather.csv",header = TRUE)
data1=na.omit(data,invert=FALSE)
attach(data1)

# Data Partitioning

index <- createDataPartition(Rain, p = 0.7, list = FALSE)
train.df <- data1[index,-c(1,20,22)]
test.df <- data1[-index,-c(1,20,21,22)]
test.Y <- data1[-index,21]

# SVM Model with Linear Kernel

model.svm <- svm(Rain ~ . , data = train.df)

pred.svm <- predict(model.svm, test.df, type = "C-classification")
head(pred.svm)
confusionMatrix(pred.svm,test.Y)

# Cross Validation 

model.cv <- train(
  Rain ~., data = train.df[,-c(1,20,22)], method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
) 
model.cv
k=model.cv$bestTune
k

pred.cv = predict(model.cv,test.df)
confusionMatrix(pred.cv,test.Y)


# SVM AND RANDOM FOREST GIVES THE BEST ACCURACY APROX. 84.1%
