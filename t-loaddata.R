# Load data

library(randomForest)
library(rpart)
library(caret)


dtrain <- read.csv("train.csv", header = TRUE, na.strings = c(" ", ""))


# cleaning data and Imputing NA values

na.cols <- sapply(dtrain[,1:dim(dtrain)[2]],anyNA)

na.cols

## head(dtrain[, !na.cols])
function ImputeData(df_train)
{
## Extract the title of the passengers

ptitle_match<-regexpr("Mr\\.|Mrs\\.|Master\\.|Miss\\.|Ms\\.",text =  dtrain$Name)

ptitles <- substr(dtrain$Name, ptitle_match, ptitle_match+attr(ptitle_match, "match.length")-2)

table(ptitles)
##    Master   Miss     Mr    Mrs     Ms 
## 26     40    182    517    125      1 

dtrain <- cbind(dtrain[,c(1:12)], data.frame("titles"=ptitles))

dtrain$titles[dtrain$titles=="" & dtrain$Sex=="male" & dtrain$Age>12 & !is.na(dtrain$Age)] <- "Mr"

dtrain$titles[dtrain$titles=="" & dtrain$Sex=="male" & dtrain$Age<13 & !is.na(dtrain$Age)] <- "Master"

# If sex=female and ((age>18 and parch=0 and SibSp=0) or (Age<19) then title=Miss
dtrain$titles[dtrain$titles=="" & (dtrain$Sex=="female" & dtrain$Age>18 & !is.na(dtrain$Age) & dtrain$Parch==0 & dtrain$SibSp==0) | (dtrain$Age<19)] <- "Miss"

# If sex=female and age>18 and (parch=1 or SibSp=1) then title=Mrs
dtrain$titles[dtrain$titles=="" & dtrain$Sex=="female" & dtrain$Age>18 & !is.na(dtrain$Age) & (dtrain$Parch==1 | dtrain$SibSp==1)] <- "Mrs"

dtrain$titles[dtrain$titles=="Ms"] <- "Miss"

dtrain$titles[dtrain$titles=="" & dtrain$Sex=="male"] <- "Mr"

dtrain$titles <- factor(dtrain$titles)

df<- data.frame(dtrain$titles, stringsAsFactors = FALSE)

grouplist <- regexpr(",",text =  dtrain$Name)

dtrain<- cbind(dtrain[,c(1:13)], data.frame(groupName = substr(dtrain$Name, 0,grouplist-1), stringsAsFactors = FALSE))

# As the majority of distribution is 'S'. We assign that to NA.
dtrain[is.na(dtrain$Embarked),"Embarked"] <- "S"

dtrain$Pclass <- factor(dtrain$Pclass)

with_age_dtrain <- dtrain[!is.na(dtrain$Age), ]
without_age_dtrain <- dtrain[is.na(dtrain$Age), ]


# Fitting glm model to impute missing Age

fit_age <- train(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+titles, data = with_age_dtrain, method="glm") 

pred_age <- predict(fit_age, without_age_dtrain)

dtrain[is.na(dtrain$Age),"Age"] <- round(pred_age)
## Alternative: dtrain[without_age_dtrain$PassengerId,"Age"] <- round(pred_age)

dtrain$Age<-round(dtrain$Age)

return(df_train)
}

