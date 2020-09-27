#  Libraries
library(dplyr)

# 1.  Merges the training and the test sets to create one data set.

#  Reading all files.
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
features <- read.table('./UCI HAR Dataset/features.txt')
activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

# select column 2 of the features data frame to add in train
colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

# select column 2 of the features data frame to add in test
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# Creating the merge in one data set
mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)

dim(setAllInOne)  # 10299   563

#  2.  Extracts only the measurements on the mean and standard deviation for each measurement.
#  Get data from setAllInOne data frame
colNames <- colnames(setAllInOne)

# grepl() function searchs for matches of a string or string vector. 
# It returns TRUE if a string contains the pattern, otherwise FALSE; 
# if the parameter is a string vector, returns a logical vector (match or not for each element of the vector). 
# It stands for "grep logical".

#  Create vector with:  activityId, subjectId, mean, and std.
mean_std <- (grepl("activityId" , colNames) | 
               grepl("subjectId" , colNames) | 
               grepl("mean.." , colNames) | 
               grepl("std.." , colNames) 
)

setForMeanStd <- setAllInOne[ , mean_std == TRUE]

#  3.  Uses descriptive activity names to name the activities in the data set
ActivityNames <- merge(setForMeanStd, 
                       activityLabels, 
                       by='activityId',
                       all.x=TRUE)

#  4.  Appropriately labels the data set with descriptive variable names.
mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)

#  Descriptive variable names below.
mean_std <- (grepl("activityId" , colNames) | 
               grepl("subjectId" , colNames) | 
               grepl("mean.." , colNames) | 
               grepl("std.." , colNames) 
)

setForMeanStd <- setAllInOne[ , mean_std == TRUE]

# 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for 
# each activity and each subject.

# Tidy data set
library(dplyr)
FinalData <- ActivityNames %>%
  group_by(subjectId, activityId) %>%
  summarise_all(funs(mean))

str(FinalData)

# Writing tidy data set in txt file
write.table(FinalData, "secondTS.txt", row.name=FALSE)