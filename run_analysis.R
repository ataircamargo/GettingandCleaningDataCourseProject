#Getting and Cleaning Data Course Project
#
#You should create one R script called run_analysis.R that does the following.
#
#Merges the training and the test sets to create one data set.
#
#Extracts only the measurements on the mean and standard deviation for each measurement.
#
#Uses descriptive activity names to name the activities in the data set
#
#Appropriately labels the data set with descriptive variable names.
#
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
#Good luck

#assign file names
filename <- "dataset.zip"
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#download dataset
if (!file.exists(filename)) {
  download.file(fileurl, filename)
}

#unzip dataset
if (!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

#select file folder and file list
filedir <- file.path("UCI HAR Dataset")
files <- list.files(path = filedir, recursive = TRUE)

#read activity files
activitytest <- read.table(file.path(filedir, "test", "y_test.txt"), header = FALSE)
activitytrain <- read.table(file.path(filedir, "train", "y_train.txt"))

#read subject files
subjecttest <- read.table(file.path(filedir, "test", "subject_test.txt"))
subjecttrain <- read.table(file.path(filedir, "train", "subject_train.txt"))

#read features files
featurestest <- read.table(file.path(filedir, "test", "x_test.txt"))
featurestrain <- read.table(file.path(filedir, "train", "x_train.txt"))

#group train and test data
subjects <- rbind(subjecttest, subjecttrain)
activities <- rbind(activitytest, activitytrain)
features <- rbind(featurestest, featurestrain)

#naming and labeling
featuresname <- read.table(file.path(filedir, "features.txt"))
names(subjects) <- c("subject")
names(activities) <- c("activities")
names(features) <- featuresname$V2

#merge all data
mergeddata <- cbind(subjects, activities)
ucihardataset <- cbind(features, mergeddata)

#subsetting wanted data (mean and stadard deviation)
subsetfeatures <- featuresname$V2[grep("mean\\(\\)|std\\(\\)", featuresname$V2)]

#subset wanted data by features names
subsetnames <- c(as.character(subsetfeatures), "subject", "activities")
subdata <- subset(ucihardataset, select = subsetnames)

#Factorize activity labels
activitylabels <- read.table(file.path(filedir, "activity_labels.txt"))
subdata$activities <-  factor(subdata$activities, levels = activitylabels[,1], labels = activitylabels[,2])

#Appropriately labels the data set with descriptive variable names
names(subdata) <- gsub("^t", "time", names(subdata))
names(subdata) <- gsub("^f", "frequency", names(subdata))
names(subdata) <- gsub("Acc", "Accelerometer", names(subdata))
names(subdata) <- gsub("Gyro", "Gyroscope", names(subdata))
names(subdata) <- gsub("BodyBody", "Body", names(subdata))

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
finaldata <- aggregate(.~subject + activities, subdata, mean)
finaldata <- finaldata[order(finaldata$subject, finaldata$activities),]
write.table(finaldata, file = "tidydata.txt", row.names = FALSE)