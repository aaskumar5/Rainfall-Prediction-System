library(caret)
library(sp)
library(class)

# Data Input

data <- read.csv("C:/Users/Magilan/Desktop/ML_project/austin_weather.csv",header = TRUE)
data1=na.omit(data,invert=FALSE)
attach(data1)

# Scalling the Data

standardized.X=scale(data1[,-c(1,20,21,22)])

# Data Partitioning

index <- createDataPartition(Rain, p = 0.7, list = FALSE)
train.X=standardized.X[index,]
test.X=standardized.X[-index,]
train.Y=Rain[index]
test.Y=Rain[-index]

# Knn Model

knn.pred=knn(train.X,test.X,train.Y,k=1)
head(data.frame(knn.pred,test.Y))
confusionMatrix(knn.pred,test.Y)

knn.pred1=knn(train.X,test.X,train.Y,k=2)
confusionMatrix(knn.pred1,test.Y)

knn.pred2=knn(train.X,test.X,train.Y,k=100)
confusionMatrix(knn.pred2,test.Y)

# Cross Validation to find the value of K with highest Accuracy

tr=cbind(standardized.X,Rain)

model <- train(
  Rain ~., data = data1[,-c(1,20,22)], method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
) 
plot(model)
k=model$bestTune
k

knn.pred3=knn(train.X,test.X,train.Y,k= model$bestTune)
confusionMatrix(knn.pred3,test.Y)



