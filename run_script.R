#script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 



# Read the data of test and train into R
subjecttrain = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subject_id"))
Xtrain = read.table("UCI HAR Dataset/train/X_train.txt")
ytrain = read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("activity_id"))  # max = 6
subjecttest = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id"))
Xtest = read.table("UCI HAR Dataset/test/X_test.txt")
ytest = read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("activity_id"))  # max = 6

## We need row number as the values of  column ID so doing the following for both test and training data
subjecttrain$ID <- as.numeric(rownames(subjecttrain))
Xtrain$ID <- as.numeric(rownames(Xtrain))
ytrain$ID <- as.numeric(rownames(ytrain))
subjecttest$ID <- as.numeric(rownames(subjecttest))
Xtest$ID <- as.numeric(rownames(Xtest))
ytest$ID <- as.numeric(rownames(ytest))

##Merging the test and training data
traindata<- merge(subjecttrain, ytrain, all=TRUE)
traindata<- merge(traindata, Xtrain, all=TRUE)
testdata <- merge(subjecttest, ytest, all=TRUE) 
testdata<- merge(testdata, Xtest, all=TRUE) 

#Result after binding the data
datasetbind <- rbind(traindata, testdata)

#Extracts only the measurements on the mean and standard deviation for each measurement.
features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)  #561
selectfeatures <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
datasetfeatures <- datasetbind[, c(c(1, 2, 3), selectfeatures$feature_id + 3) ]

#descriptive activity names to name the activities in the data set
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),) #
datasetact_features = merge(datasetfeatures, activity_labels)

#Labeling the data set
selectfeatures$feature_label = gsub("\\(\\)", "", selectfeatures$feature_label)
selectfeatures$feature_label = gsub("-", ".", selectfeatures$feature_label)
for (i in 1:length(selectfeatures$feature_label)) {
  colnames(datasetact_features)[i + 3] <- selectfeatures$feature_label[i]
}
datasetact_features_back = datasetact_features

#creating the tidy dataset
tidydata <- c("ID","activity_label")
tidydataset_label <- datasetact_features_back[,!(names(datasetact_features_back) %in% tidydata)]
aggregate_dataset <-aggregate(tidydataset_label, by=list(subject = tidydataset_label$subject_id, activity = tidydataset_label$activity_id), FUN=mean, na.rm=TRUE)
tidydata <- c("subject","activity")
aggregate_dataset <- aggregate_dataset[,!(names(aggregate_dataset) %in% tidydata)]
aggregate_dataset = merge(aggregate_dataset, activity_labels)
write.csv(file="requiredtidydataset.csv", x=aggregate_dataset)
write.table(x=aggregate_dataset,file="required_tidy_dataset.txt",row.names = FALSE)