#1Merges the training and the test sets to create one data set.
Features<- read.table('~/Desktop/UCI HAR Dataset/features.txt',header=FALSE); 
activity<- read.table('~/Desktop/UCI HAR Dataset/activity_labels.txt',header=FALSE); 
st<- read.table('~/Desktop/UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
xtrain<- read.table('~/Desktop/UCI HAR Dataset/train/x_train.txt',header=FALSE);
ytrain<- read.table('~/Desktop/UCI HAR Dataset/train/y_train.txt',header=FALSE)
colnames(activity)<- c("activity id", "activity type")
colnames(st)<- "subject id"
colnames(xtrain)<- Features[,2]
colnames(ytrain)<- "activity id"
data<- cbind(ytrain,st,xtrain)
xtest<- read.table('~/Desktop/UCI HAR Dataset/test/x_test.txt',header=FALSE)
ytest<- read.table('~/Desktop/UCI HAR Dataset/test/y_test.txt',header=FALSE)
stest<- read.table('~/Desktop/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
colnames(xtest)= Features[,2]
colnames(ytest)= "activity id"
colnames(stest)= "subject id"
datatest<- cbind(ytest, stest, xtest)
mergeddata<- rbind(data, datatest)


#2Extracts only the measurements on the mean and standard deviation for each measurement.
selectdata<- subset(mergeddata, select = c("activity id","subject id", "tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z", "tBodyAcc-std()-X","tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z", "tGravityAcc-std()-X","tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z","tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z"))

#3Uses descriptive activity names to name the activities in the data set
datawithactivity<- merge(selectdata, activity, by= "activity id", all.x = TRUE)

#4Appropriately labels the data set with descriptive variable names.
colNames<- colnames(datawithactivity)
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(datawithactivity)= colNames

#5From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata<- aggregate(datawithactivity[, names(datawithactivity) != c("activity id","subject id", "activity type")], by= list("activity id"= datawithactivity$`activity id`, "subject id" =datawithactivity$`subject id`), mean)
finaltidydata<- merge(tidydata,activity, by = "activity id", all.x = TRUE)
finaltidydata<- finaltidydata[order(finaltidydata$`subject id`),]
