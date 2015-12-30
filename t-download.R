## Download data 

# training data : https://www.kaggle.com/c/titanic/download/train.csv
# testing data : https://www.kaggle.com/c/titanic/download/test.csv

url_train <- "https://www.kaggle.com/c/titanic/download/train.csv"
url_test <- "https://www.kaggle.com/c/titanic/download/test.csv"

download.file(url_train, destfile = "train.csv", method = "auto")
download.file(url_test, destfile = "test.csv", method = "auto")

