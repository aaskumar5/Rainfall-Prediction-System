library("ISLR")
library("class")
library(caret)
library(sp)

dim(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X=scale(Caravan[,-86])


index <- createDataPartition(Caravan$Purchase, p = 0.7, list = FALSE)
train.X=standardized.X[index,-86]
test.X=standardized.X[-index,-86]
train.Y=Purchase[index]
test.Y=Purchase[-index]
knn.pred=knn(train.X,test.X,train.Y,k=1)
head(data.frame(knn.pred,test.Y))
confusionMatrix(knn.pred, test.Y)

knn.pred1=knn(train.X,test.X,train.Y,k=2)
confusionMatrix(knn.pred1, test.Y)

knn.pred2=knn(train.X,test.X,train.Y,k=100)
confusionMatrix(knn.pred2, test.Y)

tr=cbind(standardized.X,Purchase)
train1 = tr[index,]

model <- train(
  Purchase ~., data = train1, method = "knn",
  metric = "Accuracy",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
) 
plot(model)


model.X <- train(Purchase~. , data = Caravan, method = "knn",
                preProcess = c("center","scale"),
                trControl = trainControl("cv", number = 10),
                tuneLength = 10)
plot(model.X)
model.X$bestTune

model.X <- train(Purchase~. , data = Caravan, method = "bstTree",
                                    preProcess = c("center","scale"),
                                    trControl = trainControl("cv", number = 5),
                                    tuneLength = 10)



