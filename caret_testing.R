

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 20)

set.seed(1975)
mfit<- train(as.factor(Survived)~Sex+Fare+Pclass+SibSp+Women_Children+Age_Imputed+family_size+Embarked,data=train.trainset, method="C5.0",trControl=fitControl ,metric="Accuracy", tuneLength=10)

##mfit<- train(as.factor(Survived)~Sex+Fare+Pclass+Embarked,data=train.trainset, method="rf",trControl=fitControl ,metric="Accuracy")

mfit$results


p<- predict(mfit, newdata=train.testset,type = "raw")

df_test_Pred <- cbind("PassengerId"=as.numeric(train.testset$PassengerId), data.frame("predicted"=as.integer(p)-1,stringsAsFactors = FALSE),"Survived"=as.integer(train.testset$Survived)-1)


c<-confusionMatrix(df_test_Pred$predicted, df_test_Pred$Survived)
c$table

###
## Plotting Denisity Curve
###

g<-ggplot(data=df_test_Pred) + geom_density(aes(x=df_test_Pred$predicted), color="red") + geom_density(aes(x=df_test_Pred$Survived), color="blue") 
g

#------  For Running on Test Data and submitting to Kaggle ------

p<- predict(mfit, newdata=testData,type = "raw")

my_filedata <- cbind(as.numeric(testData$PassengerId), data.frame("Survived"=p))

write.csv(my_solution, file=paste0("Titanic_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)

mfit$finalModel$xNames
mfit$results
table(my_filedata$Survived)


ggplot(train.trainset, aes(Embarked, fill=Survived))+geom_bar()

0   1 
272 146 

0   1 
278 140 

0   1 
292 126 

0   1 
269 149

0   1 
268 150 

0   1 
270 148 

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

train.IL<-select(training, starts_with("IL"), diagnosis)
train2pre<-preProcess(x = train.IL, method = "pca", thresh = .80)
train2pc<-predict(train2pre, train.IL)
fit2<-train(diagnosis~.,method="glm",data=train2pc)
test2pc<-predict(train2pre,testing)
confusionMatrix(testing$diagnosis, predict(fit2, test2pc))


fit1 <- train(diagnosis~., method="glm", data=train.IL)
confusionMatrix(testing$diagnosis, predict(fit1, testing))
