##==========================================================================
## Coursera Course: Getting and Cleaning Data Peer-review Project
## Bing Hu
## 2021-01-22
##==========================================================================
## Setup project path
setwd("C://Users//bingh//Documents//JHU_Course3_DataClean")

## Setup libraries
library(dplyr)

## Get dataset
dnldzipfile <- "PeerReviewProject_data.zip"

# Checking if archieve already exists.
if (!file.exists(dnldzipfile)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, dnldzipfile, method="curl")
}  

# Unzip downloaded file into ./UCI HAR Dataset
if (!file.exists("UCI HAR Dataset")) { 
  unzip(dnldzipfile) 
}

# Read in data from unzipped files
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("n","functions"))
actlbl <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

## Read in Test data, x data use feature$Functions as variable name
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "code")

## Read in Train data, x data use feature$Functions as variable name
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "code")

## Part 1: Combine both training and test sets into one data set with all subjects 
##         (for X, Y and subject List)
X <- rbind(x_test, x_train)
Y <- rbind(y_test, y_train)
Subject <- rbind(subject_test, subject_train)
All_Data <- cbind(Subject, Y, X)

## Part 2: Extracts only the measurements on the mean and standard deviation for each measurement.
ExtData <- All_Data %>% 
    select(subject, code, contains("mean"), contains("std"))

## Part 3: Uses descriptive activity names to name the activities in the data set.
ExtData$code <- actlbl[ExtData$code, 2]
names(ExtData)[2] = "activity"

## Part 4: Appropriately labels the data set with descriptive variable names.
names(ExtData)
names(ExtData)<-gsub("Acc", "Accelerometer", names(ExtData))
names(ExtData)<-gsub("Gyro", "Gyroscope", names(ExtData))
names(ExtData)<-gsub("Mag", "Magnitude", names(ExtData))
names(ExtData)<-gsub("^t", "Time", names(ExtData))
names(ExtData)<-gsub("^f", "Frequency", names(ExtData))
names(ExtData)<-gsub("BodyBody", "Body", names(ExtData))
names(ExtData)<-gsub("-mean()", "Mean", names(ExtData), ignore.case = TRUE)
names(ExtData)<-gsub("-std()", "STD", names(ExtData), ignore.case = TRUE)
names(ExtData)<-gsub("-freq()", "Frequency", names(ExtData), ignore.case = TRUE)
names(ExtData)<-gsub("[..]", " ", names(ExtData))
names(ExtData)<-gsub("\\s+", " ", names(ExtData))

## Part 5: From the data set in step 4, creates a second, independent tidy data set with 
##        the average of each variable for each activity and each subject.
OutputData <- ExtData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(OutputData, "OutputData.txt", row.name=FALSE)


