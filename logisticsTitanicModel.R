#clean all objects from memory
rm(list=ls())

#Set working directory
setwd("C:/Users/ing12709/Desktop/Kaggle/Titanic")

library("dplyr")
options(stringsAsFactors = F)

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

test$Survived=NA
train$datasource="train"
test$datasource="test"

train_test<-rbind(train,test)

train_test$Fare[is.na(train_test$Fare)]<-mean(train_test$Age,na.rm =T)
train$Embarked[is.na(train$Embarked)]<-"C"

train_test<-mutate(train_test,Sex=as.factor(Sex),Pclass=as.factor(Pclass),Survived=as.factor(Survived),Embarked=as.factor(Embarked),Family=SibSp+Parch)

test = subset(train_test, train_test$datasource == "test")
train = subset(train_test, train_test$datasource == "train")

logisticModel <- glm(Survived ~ Pclass*Fare+Sex*Age*Family,
                     family=binomial(link="logit"))


glm.probs <- predict(logisticModel, newdata= test,type="response")

glm.pred <- ifelse(glm.probs > 0.5,1,0)

final <- data.frame(PassengerId=test$PassengerId, Survived=glm.pred)