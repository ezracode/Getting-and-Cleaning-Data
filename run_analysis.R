library(readr)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(data.table)

rm(list=ls())

print("starting analysis")
activityLabel <- read.table("activity_labels.txt", header = FALSE)
columnNames <- read.table("features.txt", header = FALSE)

xTest <- read.table(file="test/X_test.txt")
yTest <- read.table(file="test/y_test.txt")
subjectTest <- read.table(file="test/subject_test.txt")

bodyaccxTest <- read.table(file="test/Inertial Signals/body_acc_x_test.txt")
bodyaccyTest <- read.table(file="test/Inertial Signals/body_acc_y_test.txt")
bodyacczTest <- read.table(file="test/Inertial Signals/body_acc_z_test.txt")

bodygyroxTest <- read.table(file="test/Inertial Signals/body_gyro_x_test.txt")
bodygyroyTest <- read.table(file="test/Inertial Signals/body_gyro_y_test.txt")
bodygyrozTest <- read.table(file="test/Inertial Signals/body_gyro_z_test.txt")

totalaccxTest <- read.table(file="test/Inertial Signals/total_acc_x_test.txt")
totalaccyTest <- read.table(file="test/Inertial Signals/total_acc_y_test.txt")
totalacczTest <- read.table(file="test/Inertial Signals/total_acc_z_test.txt")

xTrain <- read.table(file="train/X_train.txt")
yTrain <- read.table(file="train/y_train.txt")
subjectTrain <- read.table(file="train/subject_train.txt")

bodyaccxTrain <- read.table(file="train/Inertial Signals/body_acc_x_train.txt")
bodyaccyTrain <- read.table(file="train/Inertial Signals/body_acc_y_train.txt")
bodyacczTrain <- read.table(file="train/Inertial Signals/body_acc_z_train.txt")

bodygyroxTrain <- read.table(file="train/Inertial Signals/body_gyro_x_train.txt")
bodygyroyTrain <- read.table(file="train/Inertial Signals/body_gyro_y_train.txt")
bodygyrozTrain <- read.table(file="train/Inertial Signals/body_gyro_z_train.txt")

totalaccxTrain <- read.table(file="train/Inertial Signals/total_acc_x_train.txt")
totalaccyTrain <- read.table(file="train/Inertial Signals/total_acc_y_train.txt")
totalacczTrain <- read.table(file="train/Inertial Signals/total_acc_z_train.txt")

print ("files loaded into datasets")

#step 4 phase 1: Appropriately labels the data set with descriptive variable names
columnNames <- as.data.table(columnNames)
columnNamesReport <- columnNames[ (V2 %like% "mean\\(\\)" | V2 %like% "std\\(\\)") ]

columnNamesReport$V2 <- str_replace_all(columnNamesReport$V2, "[-]", "_")
columnNamesReport$V2 <- str_replace_all(columnNamesReport$V2, "[(]", "")
columnNamesReport$V2 <- str_replace_all(columnNamesReport$V2, "[)]", "")
columnNamesReport$V2 <- tolower(columnNamesReport$V2)

columnNames$V2 <- str_replace_all(columnNames$V2, "[-]", "_")
columnNames$V2 <- str_replace_all(columnNames$V2, "[(]", "")
columnNames$V2 <- str_replace_all(columnNames$V2, "[)]", "")
columnNames$V2 <- tolower(columnNames$V2)

colnames(activityLabel) <- c("activitylabel", "activitydescription")
colnames(xTest) <- columnNames$V2
colnames(xTrain) <- columnNames$V2
colnames(yTest) <- c("activitylabel")
colnames(yTrain) <- c("activitylabel")
colnames(subjectTest) <- c("subject")
colnames(subjectTrain) <- c("subject")

#step 4 phase 2: Appropriately labels the data set with descriptive variable names
vname = "bodyacc_x"
colnames(bodyaccxTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(bodyaccxTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "bodyacc_y"
colnames(bodyaccyTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(bodyaccyTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "bodyacc_z"
colnames(bodyacczTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(bodyacczTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "bodygyro_x"
colnames(bodygyroxTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(bodygyroxTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "bodygyro_y"
colnames(bodygyroyTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(bodygyroyTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "bodygyro_z"
colnames(bodygyrozTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(bodygyrozTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "totalacc_x"
colnames(totalaccxTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(totalaccxTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "totalacc_y"
colnames(totalaccyTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(totalaccyTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

vname = "totalacc_z"
colnames(totalacczTest) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))
colnames(totalacczTrain) <- sapply(stri_pad_left(c(1:128), pad = "0", width = 3), function(x) paste(vname, x, sep = ""))

print ("colum names asigned to datasets")

datasetTest = cbind(xTest, yTest, subjectTest, bodyaccxTest, bodyaccyTest, bodyacczTest,
bodygyroxTest, bodygyroyTest, bodygyrozTest, totalaccxTest, totalaccyTest, totalacczTest)

print ("binding test datasets")

datasetTrain = cbind(xTrain, yTrain, subjectTrain, bodyaccxTrain, bodyaccyTrain, bodyacczTrain,
bodygyroxTrain, bodygyroyTrain, bodygyrozTrain, totalaccxTrain, totalaccyTrain, totalacczTrain)

print ("binding train datasets")

#Step 1: Merges the training and the test sets to create one data set.
dataset = rbind(datasetTrain, datasetTest)

print ("one dataset created")

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
datasetReport <- dataset[,c("activitylabel", "subject", columnNamesReport$V2)]

print ("Extracting mean and standard deviation")

#Step 3: Uses descriptive activity names to name the activities in the data set
datasetReport <- merge(x = datasetReport, y = activityLabel, by = "activitylabel", all = TRUE)

print ("naming the activities")

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyDataReport <- group_by(datasetReport, activitylabel, activitydescription, subject) %>%
summarise_all(funs(mean))

#step 4 phase 3: Appropriately labels the data set with descriptive variable names
tidyDataReport <- rename(tidyDataReport,
`Actiity Label Code` = activitylabel,
`Actiity Label Description` = activitydescription,
`Subject` = subject,
`Average of time domain body accelerometer signal mean axis X` = tbodyacc_mean_x,
`Average of time domain body accelerometer signal mean axis Y` = tbodyacc_mean_y,
`Average of time domain body accelerometer signal mean axis Z` = tbodyacc_mean_z,
`Average of time domain body accelerometer signal standard deviation axis X` = tbodyacc_std_x,
`Average of time domain body accelerometer signal standard deviation axis Y` = tbodyacc_std_y,
`Average of time domain body accelerometer signal standard deviation axis Z` = tbodyacc_std_z,
`Average of time domain gravity accelerometer signal mean axis X` = tgravityacc_mean_x,
`Average of time domain gravity accelerometer signal mean axis Y` = tgravityacc_mean_y,
`Average of time domain gravity accelerometer signal mean axis Z` = tgravityacc_mean_z,
`Average of time domain gravity accelerometer signal standard deviation axis X` = tgravityacc_std_x,
`Average of time domain gravity accelerometer signal standard deviation axis Y` = tgravityacc_std_y,
`Average of time domain gravity accelerometer signal standard deviation axis Z` = tgravityacc_std_z,
`Average of time domain body accelerometer jerk signal mean axis X` = tbodyaccjerk_mean_x,
`Average of time domain body accelerometer jerk signal mean axis Y` = tbodyaccjerk_mean_y,
`Average of time domain body accelerometer jerk signal mean axis Z` = tbodyaccjerk_mean_z,
`Average of time domain body accelerometer jerk signal standard deviation axis X` = tbodyaccjerk_std_x,
`Average of time domain body accelerometer jerk signal standard deviation axis X` = tbodyaccjerk_std_y,
`Average of time domain body accelerometer jerk signal standard deviation axis X` = tbodyaccjerk_std_z,
`Average of time domain body gyroscope signal mean axis X` = tbodygyro_mean_x,
`Average of time domain body gyroscope signal mean axis Y` = tbodygyro_mean_y,
`Average of time domain body gyroscope signal mean axis Z` = tbodygyro_mean_z,
`Average of time domain body gyroscope signal standard deviation axis X` = tbodygyro_std_x,
`Average of time domain body gyroscope signal standard deviation axis Y` = tbodygyro_std_y,
`Average of time domain body gyroscope signal standard deviation axis Z` = tbodygyro_std_z,
`Average of time domain body gyroscope jerk signal mean axis X` = tbodygyrojerk_mean_x,
`Average of time domain body gyroscope jerk signal mean axis Y` = tbodygyrojerk_mean_y,
`Average of time domain body gyroscope jerk signal mean axis Z` = tbodygyrojerk_mean_z,
`Average of time domain body gyroscope jerk signal standard deviation axis X` = tbodygyrojerk_std_x,
`Average of time domain body gyroscope jerk signal standard deviation axis Y` = tbodygyrojerk_std_y,
`Average of time domain body gyroscope jerk signal standard deviation axis Z` = tbodygyrojerk_std_z,
`Average of time domain body accelerometer magnitude signal mean` = tbodyaccmag_mean,
`Average of time domain body accelerometer magnitude signal standard deviation` = tbodyaccmag_std,
`Average of time domain gravity accelerometer magnitude signal mean` = tgravityaccmag_mean,
`Average of time domain gravity accelerometer magnitude signal standard deviation` = tgravityaccmag_std,
`Average of time domain body accelerometer jerk magnitude signal mean` = tbodyaccjerkmag_mean,
`Average of time domain body accelerometer jerk magnitude signal standard deviation` = tbodyaccjerkmag_std,
`Average of time domain body gyroscope magnitude signal mean` = tbodygyromag_mean,
`Average of time domain body gyroscope magnitude signal standard deviation` = tbodygyromag_std,
`Average of time domain body gyroscope jerk magnitude signal mean` = tbodygyrojerkmag_mean,
`Average of time domain body gyroscope jerk magnitude signal standard deviation` = tbodygyrojerkmag_std,
`Average of time domain body accelerometer signal mean axis X` = fbodyacc_mean_x,
`Average of time domain body accelerometer signal mean axis Y` = fbodyacc_mean_y,
`Average of time domain body accelerometer signal mean axis Z` = fbodyacc_mean_z,
`Average of time domain body accelerometer signal standard deviation axis X` = fbodyacc_std_x,
`Average of time domain body accelerometer signal standard deviation axis Y` = fbodyacc_std_y,
`Average of time domain body accelerometer signal standard deviation axis Z` = fbodyacc_std_z,
`Average of time domain body accelerometer jerk signal mean axis X` = fbodyaccjerk_mean_x,
`Average of time domain body accelerometer jerk signal mean axis Y` = fbodyaccjerk_mean_y,
`Average of time domain body accelerometer jerk signal mean axis Z` = fbodyaccjerk_mean_z,
`Average of time domain body accelerometer jerk signal standard deviation axis X` = fbodyaccjerk_std_x,
`Average of time domain body accelerometer jerk signal standard deviation axis Y` = fbodyaccjerk_std_y,
`Average of time domain body accelerometer jerk signal standard deviation axis Z` = fbodyaccjerk_std_z,
`Average of time domain body gyroscope signal mean axis X` = fbodygyro_mean_x,
`Average of time domain body gyroscope signal mean axis Y` = fbodygyro_mean_y,
`Average of time domain body gyroscope signal mean axis Z` = fbodygyro_mean_z,
`Average of time domain body gyroscope signal standard deviation axis X` = fbodygyro_std_x,
`Average of time domain body gyroscope signal standard deviation axis Y` = fbodygyro_std_y,
`Average of time domain body gyroscope signal standard deviation axis Z` = fbodygyro_std_z,
`Average of time domain body accelerometer magnitude mean`= fbodyaccmag_mean,
`Average of time domain body accelerometer magnitude standard deviation` = fbodyaccmag_std,
`Average of time domain body accelerometer jerk magnitude mean` = fbodybodyaccjerkmag_mean,
`Average of time domain body accelerometer magnitude standard deviation` = fbodybodyaccjerkmag_std,
`Average of time domain body gyroscope magnitude mean` = fbodybodygyromag_mean,
`Average of time domain body gyroscope magnitude standard deviation` = fbodybodygyromag_std,
`Average of time domain body gyroscope jerk magnitude mean` = fbodybodygyrojerkmag_mean,
`Average of time domain body gyroscope jerk magnitude standard deviation` = fbodybodygyrojerkmag_std) 

write.table(tidyDataReport, "tidyDataReport.txt", row.names = FALSE)

print ("tidy data created")

print ("ending analysis")