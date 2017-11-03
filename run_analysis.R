library(data.table)
### read the data
setwd("/Users/hai/Documents/Personal/Data_Scientist_Learning/Course 3 Getting and Cleaning Data/Getting and Cleaning Data Course Project/UCI HAR Dataset")

# import train dataset
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
train <- cbind(subject_train, y_train, X_train)

# import test dataset
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
test <- cbind(subject_test, y_test, X_test)


###
# Q1. Merges the training and the test sets to create one data set.
whole <- rbind(train,test)
dim(whole)
# [1] 10299   563


### 
# Q2. Extracts only the measurements on the mean and standard deviation for each measurement.

# get the mean() and std()
features <- read.table("./features.txt")
index <- grepl('^t',features[,2]) & (grepl("mean()",features[,2]) | grepl("std()",features[,2]))

features[index,2]

# index
X_train[,index]
X_test[,index]


# Q4. Appropriately labels the data set with descriptive variable names.

# X_train and X_test with column names
X_train_w_name <- X_train[,index]
names(X_train_w_name) <- features[index,2]
X_test_w_name <- X_test[,index]
names(X_test_w_name) <- features[index,2]

# y_train and y_test with column names
names(y_train) <- "activity"
names(y_test) <- "activity"

# subject_train and subject_test with column names
names(subject_train) <- "subject_ID"
names(subject_test) <- "subject_ID"

# combine data
train_w_name <- cbind(subject_train, y_train, X_train_w_name)
test_w_name <- cbind(subject_test, y_test, X_test_w_name)
whole_w_name <- rbind(train_w_name,test_w_name)
dim(whole_w_name)


# Q3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("./activity_labels.txt")
whole_w_name$Activity <- activity_labels[match(whole_w_name$activity,activity_labels$V1),2]
whole_w_name_activities <- subset(whole_w_name, select = -c("activity"))
whole_w_name_activities <- whole_w_name[, c(-2)]

# Q5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- data.table(whole_w_name_activities)
table_average <- tidy_data[,lapply(.SD,mean),by=c("subject_ID","Activity")]
write.table(table_average, file = "Average_table_for_Q_5.csv",sep = ",")
