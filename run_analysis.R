library(data.table)

# obtain data
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "uci_har_dataset.zip" 

if (!file.exists(fileName)) { 
   print("downloading dataset...")
   download.file(fileUrl, destfile = fileName, method = "curl") 
}

if (!file.exists("UCI HAR Dataset")) { 
   print("unzipping dataset...")
   unzip(fileName)
}

# features 
features <- read.table("UCI HAR Dataset/features.txt")

# merge train and test sets and add column names

# x
data.train        <- read.table("UCI HAR Dataset/train/X_train.txt")
data.test         <- read.table("UCI HAR Dataset/test/X_test.txt")
data              <- rbind(data.train, data.test)
colnames(data)    <- unlist(lapply(features[,2], FUN=function(x) {  
                        x<-tolower(x)
                        x<-gsub("^f","frequency_",x);
                        x<-gsub("^t","time_",x); x
                        x<-gsub("\\-","\\_",x); 
                        x<-gsub("\\,","\\_",x); 
                        x<-gsub("\\(|\\)","_",x); 
                        x<-gsub("body|bodybody","body_",x);
                        x<-gsub("gravity","gravity_",x);
                        x<-gsub("mag","magnitude_",x);
                        x<-gsub("gyro","gyroscope_",x);
                        x<-gsub("acc","acceleration_",x);
                        x<-gsub("jerk","jerk_",x);
                        x<-gsub("(_)+","_",x);
                        x<-gsub("_$","",x);
}))

# y
labels.train         <- read.table("UCI HAR Dataset/train/Y_train.txt")
labels.test          <- read.table("UCI HAR Dataset/test/Y_test.txt")
labels               <- rbind(labels.train, labels.test)
colnames(labels)     <- c("activity_id")

# subject
subject.train        <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject.test         <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject              <- rbind(subject.train, subject.test)
colnames(subject)    <- c("subject_id")

# combine columns
data.raw <- cbind(subject, data, labels)

# extract only measurements on the mean and standard deviation
tidy.data <- data.table(data.raw[, grep("activity_id|subject_id|(^(frequency|time).*(mean|std)(_|$))", names(data.raw))])

# use descriptive activity names instead of id
activity <- data.table(read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_name")), key="activity_id")
tidy.data[, activity_name := activity[activity_id]$activity_name]
# remove activity_id column
tidy.data[, activity_id := NULL]
write.table(tidy.data, file="sensor_means_and_std.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)

# summarize data by activity and subject
tidy.data.activity.subject <- tidy.data[, lapply(.SD, mean), by=list(activity_name, subject_id)]
write.table(tidy.data.activity.subject, file="sensor_means_and_std_grouped_by_activity_and_subject.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)