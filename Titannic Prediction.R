# Your train and train.testset set are still loaded in
str(train)
str(train.testset)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, method="class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancified tree
fancyRpartPlot(my_tree_two)

# Make your prediction using the train.testset set
my_prediction <- predict(my_tree_two, newdata=train.testset, "class")

df<-data.frame(my_prediction)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = train.testset$PassengerId, Survived = df[,1])

# Check that your data frame has 418 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file="my_solution.csv" , row.names=FALSE)


# Time to improve the Model even more

# Create a new decision tree my_tree_three
my_tree_three <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, method="class", control=rpart.control(minsplit=50, cp=0))

# Visualize your new decision tree
fancyRpartPlot(my_tree_three)

### Adding new variable

# create a new train set with the new variable
train_two <- train
train_two$family_size <- (train_two$SibSp + train_two$Parch+1)

# Create a new decision tree my_tree_three
my_tree_four <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+family_size, data=train_two, method="class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)


### Adding variable Title

# train_new and train.testset_new are available in the workspace
str(train_new)
str(train.testset_new)

# Create a new model `my_tree_five`
my_tree_five <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title,train_new, method="class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_five)

# Make your prediction using `my_tree_five` and `train.testset_new`
my_prediction <- predict(my_tree_five, newdata=train.testset_new, type="class")

df<-data.frame(my_prediction)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = train.testset$PassengerId, Survived = df[,1])

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file="my_solution.csv" , row.names=FALSE)

##
## Random Forest
##

# All data, both training and train.testset set
all_data

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(combi$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a train.testset set
train <- all_data[1:891,]
train.testset <- all_data[892:1309,]


##
##
##

# train and train.testset are available in the workspace
str(train)
str(train.testset)

# Load in the package
library(randomForest)

# Train set and train.testset set
str(train)
str(train.testset)

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title, data=train, importance=TRUE, ntree=1000)

# Make your prediction using the train.testset set
my_prediction <- predict(my_forest, train.testset)

df<-data.frame(my_prediction)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = train.testset$PassengerId, Survived = df[,1])

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file="my_solution.csv", row.names=FALSE)


##-----------------------------------------------------------
##

## Imputing AGE where Missing/NA
##

library(dplyr)
library(rpart)
library(randomForest)
library(caret)

train<-read.csv("train.csv")


train<-tbl_df(train)

train<- mutate(train, Age_Imputed=Age)

#Partition Data into Train and train.testset sets for Model evaluation

t<-createDataPartition(train$PassengerId, p=0.7, list = FALSE)

train.trainset <- train[t,]
train.testset <- train[-t,]


pnames <- strsplit(as.character(train.trainset$Name), split = c(" "), fixed = TRUE)

### Alternative -- df_suffix<-as.data.frame(unlist(lapply(pnames, "[[", 2)))

df_names<-as.data.frame(unlist(pnames))
head(sort(table(df_names[,1]), decreasing = TRUE), 20)

df_namecounts<-as.data.frame(sort(table(df_names[,1]), decreasing = TRUE))

## Individuals with valid suffix

df_namecounts[c("Mr.", "Mrs.", "Master.", "Miss.", "Dr."),]
## Mr.    Mrs. Master.   Miss.     Dr.      Jr 
## 517     125      40     182       7      10 

## Number of Individuals with valid suffix    ---871
sum(df_namecounts[c("Mr.", "Mrs.", "Master.", "Miss.", "Dr."),])

## Number of Individuals without suffix  ----20

dim(train.trainset)[1]-sum(df_namecounts[c("Mr.", "Mrs.", "Master.", "Miss.", "Dr."),])

## Individuals with and without suffix at all.
train[grep("Mr\\.|Mrs\\.|Master\\.|Miss\\.|Dr\\.", train.trainset$Name, value = FALSE),]
train[-grep("Mr\\.|Mrs\\.|Master\\.|Miss\\.|Dr\\.", train.trainset$Name, value = FALSE),]

## Extract "Age" for individuals with Suffix "Mr."   dim---517,12
df_Mr<-train.trainset[grep("Mr\\.", train.trainset$Name, ignore.case = FALSE),]
head(arrange(df_Mr, Age))
## We see that passengerID 732 has Age 11 but Suffix Mr. So assuming that age was a typo, update the Age to 12.
train[train.trainset$PassengerId==732,"Age_Imputed"] = 12

df_Master<-train.trainset[grep("Master\\.", train.trainset$Name, ignore.case = FALSE),]
df_Master <- tbl_df(df_Master)


df_Miss<-train.trainset[grep("Miss\\.", train.trainset$Name, ignore.case = FALSE),]
df_Mrs<-train.trainset[grep("Mrs\\.", train.trainset$Name, ignore.case = FALSE),]
df_Dr<-train.trainset[grep("Dr\\.", train.trainset$Name, ignore.case = FALSE),]

df_Miss <- tbl_df(df_Miss)
df_Mrs <- tbl_df(df_Mrs)
df_Dr <- tbl_df(df_Dr)

df_Mr$Age<-as.numeric(df_Mr$Age)
df_Master$Age<-as.numeric(df_Master$Age)
df_Miss$Age<-as.numeric(df_Miss$Age)
df_Mrs$Age<-as.numeric(df_Mrs$Age)
df_Dr$Age<-as.numeric(df_Dr$Age)


## Number of individuals with Age=NA with Suffix "Mr."   --- 119
sum(is.na(train.trainset[grep("Mr\\.", train.trainset$Name), "Age"]))
sum(is.na(df_Mr$Age))

## Individuals with Age=NA with Suffix "Mr."
df_Mr[is.na(df_Mr$Age),]

## Individuals with Age<>NA with Suffix "Mr."   ---398
df_Mr[!is.na(df_Mr$Age),]



## Histogram of Age distribution of person with Suffix "Mr."
hist(df_Mr$Age)
hist(df_Miss$Age)
hist(df_Mrs$Age)
hist(df_Master$Age)
hist(df_Dr$Age)

excess_factor <- 1.2

##### Imputing Mr.
imp<-rnorm(sum(is.na(df_Mr$Age))*excess_factor,mean(df_Mr$Age[!is.na(df_Mr$Age)]),sd(df_Mr$Age[!is.na(df_Mr$Age)]))

  imp<-imp[imp>12][1:sum(is.na(df_Mr$Age))]


  train.trainset$Age_Imputed[grepl("Mr\\.", train.trainset$Name) & is.na(train.trainset$Age)]<-as.integer(imp)

#### ----- ####
  
  
  ##### Imputing Mrs.
  imp<-rnorm(sum(is.na(df_Mrs$Age))*excess_factor,mean(df_Mrs$Age[!is.na(df_Mrs$Age)]),sd(df_Mrs$Age[!is.na(df_Mrs$Age)]))
  
  imp<-as.integer(imp[imp>21][1:sum(is.na(df_Mrs$Age))])
  
  
  train.trainset$Age_Imputed[grepl("Mrs\\.", train.trainset$Name) & is.na(train.trainset$Age)]<-as.integer(imp)
  
  #### ----- ####
  
  ##### Imputing Miss.
  imp<-rnorm(sum(is.na(df_Miss$Age))*excess_factor,mean(df_Miss$Age[!is.na(df_Miss$Age)]),sd(df_Miss$Age[!is.na(df_Miss$Age)]))
  
  imp<-as.numeric(imp[imp>0][1:sum(is.na(df_Miss$Age))])
  imp<-as.integer(imp[imp>1])
  
  train.trainset$Age_Imputed[grepl("Miss\\.", train.trainset$Name) & is.na(train.trainset$Age)]<-imp
  #### ----- ####
  
  
  ##### Imputing Master.
  imp<-rnorm(sum(is.na(df_Master$Age))*excess_factor,mean(df_Master$Age[!is.na(df_Master$Age)]),sd(df_Master$Age[!is.na(df_Master$Age)]))
  
  imp<-as.numeric(imp[imp<=12 & imp>0][1:sum(is.na(df_Master$Age))])
  imp<-as.integer(imp[imp>1])
  
  
  train.trainset$Age_Imputed[grepl("Master\\.", train.trainset$Name) & is.na(train.trainset$Age)]<-imp
  #### ----- ####
  
  ##### Imputing Dr.
  imp<-rnorm(sum(is.na(df_Dr$Age))*excess_factor,mean(df_Dr$Age[!is.na(df_Dr$Age)]),sd(df_Dr$Age[!is.na(df_Dr$Age)]))
  
  imp<-as.integer(imp[imp>25][1:sum(is.na(df_Dr$Age))])
  
  
  train.trainset$Age_Imputed[grepl("Dr\\.", train.trainset$Name) & is.na(train.trainset$Age)]<-(imp)
  #### ----- ####
  
  ###--- Looking at the Mean of Fares of the persons boarding at S, C and Q. We allocate "C" as the embarkation code for the missing value. The missing value fare = 80
  train.trainset$Embarked[train.trainset$Embarked==""]="C"
  train.trainset$Embarked <- factor(train.trainset$Embarked)
  
  ### Now evaluate how many families were travelling and family size.
  train.trainset$family_size <- (train.trainset$SibSp + train.trainset$Parch+1)
  
  ## Impute Fare for 2 missing values
  ##  Check Data
  train.trainset[is.na(train.trainset$Fare),]
  train.trainset$Fare<-as.numeric(train.trainset$Fare)
  train.trainset[train.trainset$Embarked=="C" & !is.na(train.trainset$Fare) & train.trainset$family_size==1 & grepl("Mrs\\.", train.trainset$Name),c("Name", "Age","Age_Imputed", "Fare", "family_size", "SibSp","Parch")]
  train.trainset[train.trainset$Embarked=="C" & !is.na(train.trainset$Fare) & train.trainset$family_size==1 & grepl("Miss\\.", train.trainset$Name),c("Name", "Age","Age_Imputed", "Fare", "family_size", "SibSp","Parch")]
  
  train.trainset$Fare[is.na(train.trainset$Fare) & grepl("Mrs\\.", train.trainset$Name)] = mean(train.trainset$Fare[train.trainset$Embarked=="C" & !is.na(train.trainset$Fare) & train.trainset$family_size==1 & grepl("Mrs\\.", train.trainset$Name)])
  train.trainset$Fare[is.na(train.trainset$Fare) & grepl("Miss\\.", train.trainset$Name)] = mean(train.trainset$Fare[train.trainset$Embarked=="C" & !is.na(train.trainset$Fare) & train.trainset$family_size==1 & grepl("Miss\\.", train.trainset$Name)])
  
  
  # Create a new decision tree my_tree_three
  set.seed(102)
  my_tree <- rpart(Survived ~ Sex+Fare+Age_Imputed+Pclass+SibSp+Parch+family_size+Embarked, data=train.trainset, method="class")
  
  ##write.csv(train.trainset, file="train.trainset_set.csv" , row.names=FALSE)
  
  
  ### ---------------------------------------------- ###
  ### ---- Time to process train.testset Data   ------------  ###
  ### ---------------------------------------------- ###
  
  ## --- Commenting as we are evaluating the model on the split train-test data.  ---- ##
  
  ##train.testset<-read.csv("train.testset.csv")  
  
  ##train.testset<-tbl_df(train.testset)
  train.testset$Fare<-as.numeric(train.testset$Fare)
  train.testset$family_size<-(train.testset$SibSp + train.testset$Parch+1)
  ##train.testset$Age_Imputed<-train.testset$Age  
  
  
  #----x----x----x---x-----#
  
  set.seed(101)
  excess_factor <- 1.5
  
  df_Mr<-train.testset[grep("Mr\\.", train.testset$Name, ignore.case = FALSE),]
  df_Master<-train.testset[grep("Master\\.", train.testset$Name, ignore.case = FALSE),]
  df_Miss<-train.testset[grep("Miss\\.", train.testset$Name, ignore.case = FALSE),]
  df_Mrs<-train.testset[grep("Mrs\\.", train.testset$Name, ignore.case = FALSE),]
  df_Dr<-train.testset[grep("Dr\\.", train.testset$Name, ignore.case = FALSE),]
  
  df_Mr <- tbl_df(df_Mr)
  df_Master <- tbl_df(df_Master)
  df_Miss <- tbl_df(df_Miss)
  df_Mrs <- tbl_df(df_Mrs)
  df_Dr <- tbl_df(df_Dr)  
  
  ##### Imputing Mr.
  imp<-rnorm(sum(is.na(df_Mr$Age))*excess_factor,mean(df_Mr$Age[!is.na(df_Mr$Age)]),sd(df_Mr$Age[!is.na(df_Mr$Age)]))
  
  imp<-imp[imp>12][1:sum(is.na(df_Mr$Age))]
  
  
  train.testset$Age_Imputed[grepl("Mr\\.", train.testset$Name) & is.na(train.testset$Age)]<-as.integer(imp)
  
  #### ----- ####
  
  
  ##### Imputing Mrs.
  imp<-rnorm(sum(is.na(df_Mrs$Age))*excess_factor,mean(df_Mrs$Age[!is.na(df_Mrs$Age)]),sd(df_Mrs$Age[!is.na(df_Mrs$Age)]))
  
  imp<-as.integer(imp[imp>21][1:sum(is.na(df_Mrs$Age))])
  
  
  train.testset$Age_Imputed[grepl("Mrs\\.", train.testset$Name) & is.na(train.testset$Age)]<-as.integer(imp)
  
  #### ----- ####
  
  ##### Imputing Miss.
  imp<-rnorm(sum(is.na(df_Miss$Age))*excess_factor,mean(df_Miss$Age[!is.na(df_Miss$Age)]),sd(df_Miss$Age[!is.na(df_Miss$Age)]))
  
  imp<-as.numeric(imp[imp>0][1:sum(is.na(df_Miss$Age))])
  imp<-as.integer(imp[imp>1])
  
  train.testset$Age_Imputed[grepl("Miss\\.", train.testset$Name) & is.na(train.testset$Age)]<-imp
  #### ----- ####
  
  
  ##### Imputing Master.
  imp<-rnorm(sum(is.na(df_Master$Age))*excess_factor,mean(df_Master$Age[!is.na(df_Master$Age)]),sd(df_Master$Age[!is.na(df_Master$Age)]))
  
  imp<-as.numeric(imp[imp<=12 & imp>0][1:sum(is.na(df_Master$Age))])
  imp<-as.integer(imp[imp>1])
  
  
  train.testset$Age_Imputed[grepl("Master\\.", train.testset$Name) & is.na(train.testset$Age)]<-imp
  #### ----- ####
  
  ##### Imputing Dr.
  imp<-rnorm(sum(is.na(df_Dr$Age))*excess_factor,mean(df_Dr$Age[!is.na(df_Dr$Age)]),sd(df_Dr$Age[!is.na(df_Dr$Age)]))
  
  imp<-as.integer(imp[imp>25][1:sum(is.na(df_Dr$Age))])
  
  
  train.testset$Age_Imputed[grepl("Dr\\.", train.testset$Name) & is.na(train.testset$Age)]<-(imp)
  #### ----- ####
  
  
  train.testset[train.testset$Embarked=="S" & !is.na(train.testset$Fare) & train.testset$family_size==1 & grepl("Mr\\.", train.testset$Name),c("Name", "Age","Age_Imputed", "Fare", "family_size", "SibSp","Parch")]
  
  train.testset$Fare[train.testset$Embarked=="S" & is.na(train.testset$Fare) & train.testset$family_size==1 & grepl("Mr\\.", train.testset$Name)] = mean(train.testset$Fare[train.testset$Embarked=="S" & !is.na(train.testset$Fare) & train.testset$family_size==1 & grepl("Mr\\.", train.testset$Name)])
  
  train.testset$Age_Imputed[grepl("Ms\\.", train.testset$Name) & is.na(train.testset$Age)]<-11
  
  train.testset[train.testset$Embarked=="","Embarked"] = "C"
  train.testset$Embarked <- factor(train.testset$Embarked)
  
  ## ---- Write out imputed Test Set
  ##write.csv(x = train.testset, file = "train.testset_set_imputed.csv", row.names = FALSE)
  
  ##train.testset$Survived<-0
  
  # Make your prediction using `my_tree_five` and `train.testset_new`
  my_prediction <- predict(my_tree, newdata=train.testset, type="class")
  

  df<-data.frame(my_prediction)
  
  # Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
  my_solution <- data.frame(PassengerId = train.testset$PassengerId, Survived = df[,1])
  
  # Write your solution away to a csv file with the name my_solution.csv
  ##write.csv(my_solution, file=paste0("my_solution_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)
  
  confusionMatrix(df$my_prediction , train.testset$Survived)
  
  ## ----------------------
  ## Random Forest
  
  library(randomForest)
  
  # Set seed for reproducibility
  set.seed(101)
  
  # Apply the Random Forest Algorithm
  ##
  ## my_forest <- randomForest(as.factor(Survived)~Pclass+Sex+Age_Imputed+SibSp+Parch+Fare+Embarked+family_size, data=train, importance=TRUE, ntree=2000)

  my_forest <- randomForest(as.factor(Survived)~Sex+Age_Imputed+Fare+SibSp+Parch+Embarked, data=train.trainset, importance=TRUE, ntree=5000)
  
  # Make your prediction using the train.testset set
  my_prediction <- predict(my_forest, train.testset)
  
  df<-data.frame(my_prediction)
  
  confusionMatrix(df$my_prediction , train.testset$Survived)
  
  train.testset$Survived<-df[,1]
  # Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
  my_solution <- data.frame(PassengerId = train.testset$PassengerId, Survived = df[,1])
  
  # Write your solution away to a csv file with the name my_solution.csv
  ##write.csv(my_solution, file="my_solution_11192015_b.csv" , row.names=FALSE)
  


  