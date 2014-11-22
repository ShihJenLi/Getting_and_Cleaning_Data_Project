setInternet2(TRUE) 

 ##if (!file.exists("UCI HAR Dataset")) { dir.create("UCI HAR Dataset")}

  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile = "Dataset.zip")
  
  unzip("Dataset.zip")

setwd("./UCI HAR Dataset"); list.files()


#0.read subject, activity, and features data

Sub_train<-read.table("./train/subject_train.txt")
Sub_test<-read.table("./test/subject_test.txt")
activity_lables<-read.table("./activity_labels.txt")
features<-read.table("./features.txt")

#1.load train/ test data

train_data.x<-read.table("./train/X_train.txt")
test_data.x<-read.table("./test/X_test.txt")
train_data.y<-read.table("./train/y_train.txt")
test_data.y<-read.table("./test/y_test.txt")

# combine training and test data

all_data.x<-rbind(train_data.x,test_data.x)
all_data.y<-rbind(train_data.y,test_data.y)

# lookup mean() and STD() variables

colnames(all_data.x) <- c(as.character(features[,2]))
Mean_x<-grep("mean()",colnames(all_data.x),fixed=TRUE)
SD_x<-grep("std()",colnames(all_data.x),fixed=TRUE)
MeanSD_x<-all_data.x[,c(Mean_x,SD_x)]

# Combine activity to variable data
all.activity<-cbind(all_data.y,MeanSD_x)
colnames(all.activity)[1] <- "Activity"
activity_lables[,2]<-as.character(activity_lables[,2])

for(i in 1:length(all.activity[,1])){
  all.activity[i,1]<-activity_lables[all.activity[i,1],2]
}

#Create the tidy data set

Sub_all<-rbind(Sub_train,Sub_test)
all_data<-cbind(Sub_all,all.activity)
colnames(all_data)[1] <- "Subject"

Tidy_data <- aggregate( all_data[,3] ~ Subject+Activity, data = all_data, FUN= "mean" )

# Average activity data by subjects

for(i in 4:ncol(all_data)){
  Tidy_data[,i] <- aggregate( all_data[,i] ~ Subject+Activity, data = all_data, FUN= "mean" )[,3]
}

colnames(Tidy_data)[3:ncol(Tidy_data)] <- colnames(MeanSD_x)

#Export the tidy data set
write.table(Tidy_data, file = "TidyData.txt", row.name = FALSE)
