library(data.table)
library(reshape2)
library(reshape)
library(plyr)
library(doBy)


#READ IN THE LIST OF COLUMN/VARIABLE NAMES AS A DATA TABLE
#Ensure to set your directory to the appropriate folder:
setwd("~/Coursera Courses/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

VarNames <- data.table(read.table("features.txt", sep="", header=FALSE))
VarNames$V1 <- NULL
setnames(VarNames, "V2","Column.Names")
VarNames2 <- sub("\\-",".",VarNames$Column.Names)
VarNames3 <- sub("\\()",".",VarNames2)
VarNames4 <- sub("\\-",".",VarNames3)


#READ IN THE ACTIVITY LABELS(NAMES): This links the class labels found in the "TestSetLabels" and "TrainSetLabels" data tables 
#with the actual description of the activity name

ActivityNames <- data.table(read.table("activity_labels.txt", sep="", header=FALSE))
setnames(ActivityNames,"V1","Activity.Label")
setnames(ActivityNames,"V2","Activity.Name")



#READ IN THE TEST SET AND THE TEST LABELS AS DATA TABLES
#Ensure to set your directory to the appropriate folder:
setwd("~/Coursera Courses/Getting and Cleaning Data/Course Project/UCI HAR Dataset/test")

TestSet <- data.table(read.table("X_test.txt", sep="", header=FALSE))
TestSet2 <- transform(TestSet, Data.Type= "Test")
  
TestSetLabels <-data.table(read.table("y_test.txt", sep="", header=FALSE))
setnames(TestSetLabels,"V1","Activity.Label")

TestSet3 <- cbind(TestSet2,TestSetLabels)


#READ IN THE FILE CONTAINING THE TEST SUBJECT IDENTIFIERS
SubjectTest <- data.table(read.table("subject_test.txt", sep="", header=FALSE))
SubjectTest2 <- transform(SubjectTest, Subject.ID=paste("subject",V1,sep="."))
SubjectTest2$V1 <- NULL
TestSet4 <- cbind(TestSet3,SubjectTest2)





#READ IN THE TRAINING SET AND TRAINING LABELS
#Ensure to set your directory to the appropriate folder:
setwd("~/Coursera Courses/Getting and Cleaning Data/Course Project/UCI HAR Dataset/train")

TrainSet <- data.table(read.table("X_train.txt", sep="", header=FALSE))
TrainSet2 <- transform(TrainSet, Data.Type= "Train")

TrainSetLabels <- data.table(read.table("y_train.txt", sep="", header=FALSE))
setnames(TrainSetLabels,"V1","Activity.Label")

TrainSet3 <- cbind(TrainSet2,TrainSetLabels)

#READ IN THE FILE CONTAINING THE TEST SUBJECT IDENTIFIERS
SubjectTrain <- data.table(read.table("subject_train.txt", sep="", header=FALSE))
SubjectTrain2 <- transform(SubjectTrain, Subject.ID=paste("subject",V1,sep="."))
SubjectTrain2$V1 <- NULL
TrainSet4 <- cbind(TrainSet3, SubjectTrain2)


#COURSE PROJECT OBJECTIVE #1: Merges the training and the test sets to create one data set.
#COURSE PROJECT OBJECTIVE #3: Uses descriptive activity names to name the activities in the data set
CompleteData <- rbind(TestSet4, TrainSet4)

CompleteData2 <- merge(CompleteData, ActivityNames, by="Activity.Label", all.x=TRUE)
CompleteData2$Activity.Label <- NULL



#COURSE PROJECT OBJECTIVE #4: Appropriately labels the data set with descriptive variable names. 

for(i in 1:561) {
  setnames(CompleteData2,paste(names(CompleteData2)[i]),paste(VarNames4[i]))
}


#COURSE PROJECT OBJECTIVE #2: Extracts only the measurements on the mean and standard deviation for each measurement. 
#SUBSET MY COMPLETE DATA SET BY SELECTING ONLY THOSE COLUMNS THAT ARE 'MEAN' OR 'STANDARD DEVIATION' MEASUREMENTS
CompleteData3 <- as.data.frame(CompleteData2)

CompleteData4 <- CompleteData3[,grepl( "std|mean" , names( CompleteData3) )]
DataTypeAndActName <- subset(CompleteData3, select=c("Data.Type","Activity.Name","Subject.ID"))


#TIDY DATA SET 1: satisfies the requirements for Course Objectives 1-4
TidyDataBig <- cbind(CompleteData4, DataTypeAndActName)

#TIDY DATA SET 2: satisfies the requirements Course Objective 5: includes the average of each variable for each activity and each subject
#Mean of each variable will be done by activity AND subject, irrespective of data type (train or test)
TidyData <- summaryBy( . ~ Activity.Name + Subject.ID, data = TidyDataBig, FUN=mean, na.rm=TRUE) 

#WRITE TIDY DATA SET 2 INTO FOLDER FOR COURSE PROJECT AS A COMMA-SEPARATED (",") TEXT FILE
#Ensure to set your directory to the appropriate folder:
setwd("~/Coursera Courses/Getting and Cleaning Data/Course Project")
write.table(TidyData, "TidyData.txt", sep=",")




