library(tidyverse)
library(boot)
library(forecast)
library(tseries) 
library(caret)
library(ROCR)
library(corrplot)
library(psych)
library(devtools)
library(ggbiplot)
library(sp)
library(class)


data <- read.csv("C:/Users/Magilan/Desktop/ML_project/austin_weather.csv",header = TRUE)
data1=na.omit(data,invert=FALSE)
attach(data1)

# Principal Component analysis

pc = prcomp(data1[,-c(1,20,21,22)],
            center=TRUE,
            scale. = TRUE)
pc$center
summary(pc)

# Orthogonality of PC

pairs.panels(pc$x,gap=0,pch=21)

g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = data1$Rain,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

pc.df=data.frame(pc$x)

index <- createDataPartition(Rain, p = 0.7, list = FALSE)
# Training set
train.df <- pc.df[index,]
train.Y = data1[index,22]
train.Y1 = data1[index,21]
train = cbind(train.df,train.Y)

# Testing dataset
test.df <- pc.df[-index,]
test.Y = data1[-index,22]
test.Y1 =data1[-index,21]
test = cbind(test.df,test.Y)

# Logistic Regression With PCA

model <- glm(train$train.Y ~. , data = train)
summary(model)

predicted_values <- predict(model, test.df, type = "response")
head(predicted_values)

#Vlaidation
table(Rain)
nrows_prediction<-nrow(test.df)
prediction <- data.frame(c(1:nrows_prediction))
colnames(prediction) <- c("Rain")
str(prediction)
prediction$Rain <- as.character(prediction$Rain)
prediction$Rain <- "yes"
prediction$Rain[ predicted_values < 0.5] <- "no"
prediction$Rain <- as.factor(prediction$Rain)

#Confusion Matrix

table(prediction$Rain, test.Y1)
confusionMatrix(prediction$Rain,test.Y1)

#Plotting

ggplot(test, aes(x = test.df$PC1, y = predicted_values))+
  geom_point() + # add points
  geom_smooth(method = "lm", # plot a regression...
              method.args = list())


# KNN After PCA


model.knn = knn(train.df,test.df,train.Y1,k=1)
head(data.frame(model.knn,test.Y1))
confusionMatrix(model.knn,test.Y1)

tr=cbind(pc.df,Rain)

model.cv <- train(
  Rain ~., data = tr, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
) 
plot(model.cv)
K=model.cv$bestTune
K

model.knn = knn(train.df,test.df,train.Y1,k=K)
head(data.frame(model.knn,test.Y1))
confusionMatrix(model.knn,test.Y1)
