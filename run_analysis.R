library("data.table")
library(lubridate)
library(quantmod)
library(plyr)
library(dplyr)
#Primer punto
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "dataset.zip")

unzip("dataset.zip")
setwd("UCI HAR Dataset")


Train_x <- read.table("train/X_train.txt")
Test_x <- read.table("test/X_test.txt")
X <- rbind(Train_x, Test_x)


train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
subject <- rbind(train, test)


train_y <- read.table("train/y_train.txt")
test_y <- read.table("test/y_test.txt")
Y <- rbind(train_y, test_y)


#segundo punto

features <- read.table("features.txt", colClass = "character")
features.selected <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, features.selected] 
names(X) <- features[features.selected, 2]
names(X) <- gsub("\\(|\\)", "", names(X))  
names(X) <- gsub("\\-", " ", names(X))

#Tecer punto

actividad <- read.table("activity_labels.txt")
Y[,1] <- actividad[Y[,1], 2]
names(Y) <- "activity"

#Cuarto punto
names(subject) <- "subject"
resultado <- cbind(subject, Y, X)
write.table(resultado,  "merged_and_cleaned_dataset.txt")
#quinto punto

subjects.unique <- unique(subject)[,1]
subjec_longitud <-  length(subjects.unique)
actividad_longitud <- length(actividad[,1])
columns <- ncol(resultado)

data.tidy <- resultado[1:(subjec_longitud * actividad_longitud), ]
row <- 1
for (s in 1:actividad_longitud) {
  for (a in 1:actividad_longitud) {
    data.tidy[row, 1] <- subjects.unique[s]
    data.tidy[row, 2] <- actividad[a, 2]
    subset <- resultado[resultado$subject==s & 
                             resultado$activity==actividad[a, 2], ]
    data.tidy[row, 3:columns] <- colMeans(subset[, 3:columns])
    row <- row+1
  }
}
write.table(data.tidy, "tidy.txt", row.names = FALSE)


