# Process Test Data

dtest <- read.csv("test.csv", header = TRUE)

dtest <- ImputeData(dtest)


pred.rf <- predict(fit.rf, dtest)
pred.gbm <- predict(fit.gbm, dtest)
pred.rpart <- predict(fit.rpart, dtest)

df_pred <- cbind(data.frame(pred.rf=pred.rf), data.frame(pred.gbm=pred.gbm), data.frame(pred.rpart=pred.rpart))

pred_test <- predict(fit_combo, df_pred)

my_solution <- data.frame(PassengerId = dtest$PassengerId, data.frame(Survived=pred_test))

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file=paste0("Titanic_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)


