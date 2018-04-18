#------------------------------------------------------------------------------
# This script takes data from UCI Machine Learning Repository site and downloads
# files on Humna Activity Recognition Using Smartphones Data Set.
#
# This script will:
# 1. Merge the training and test set to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each
#    measurement.
# 3. Use descritptive activity names to name the activityies in the data set
# 4. Appropriately labe the data set with descriptive variable names
# 5. From the data set that is created a second independent tidy data set with
#    the average of each variable for each activity and each subject.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Get files and setup paths and file names.
#------------------------------------------------------------------------------
path<-getwd()
# Load Packages and get the Data
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")
projectPath<-paste(path,"/UCI HAR Dataset",sep="")
testPath<-paste(projectPath, "/test", sep="")
trainPath<- paste(projectPath, "/train",sep="")
activityLabelsPath<-paste(projectPath,"/activity_labels.txt", sep="")
featuresePath<-paste(projectPath,"/features.txt", sep="")
testActivityFilePath<-paste(testPath, "/y_test.txt", sep="")
testSubjectPath<-paste(testPath, "/subject_test.txt", sep="")
testMeasurementFilePath<-paste(testPath,"/X_test.txt",sep="")
trainActivityFilePath<-paste(trainPath, "/y_train.txt", sep="")
trainSubjectPath<-paste(trainPath, "/subject_train.txt", sep="")
trainMeasurementFilePath<-paste(trainPath,"/X_train.txt",sep="")

#------------------------------------------------------------------------------
# Get the data that are common to both the test and training data. Rename 
# columns for readability and understandability
#------------------------------------------------------------------------------
activityLabels <- read.table(activityLabelsPath)
names(activityLabels)<- c("activityIndex","activityName")

# Get the column names from the feature.txt file, going to rename columns that 
# are needed later.
features<- read.table(featuresePath)

#------------------------------------------------------------------------------
# Get the files for test data and combine data files together so that there is a
# single file for test data with the type of data (test), subject, activityLabel 
# and measurements for each of the variables. 
#------------------------------------------------------------------------------
testActivity<-read.table(testActivityFilePath, header = FALSE)
names(testActivity)<-c("activityIndex")
testSubject <- read.table(testSubjectPath, header = FALSE)
names(testSubject)<- c("subject")
testMeasurement<-read.table(testMeasurementFilePath, header = FALSE)
names(testMeasurement)<-features$V2

# Combine Suject, Activity and Measurement into test data
testData<-cbind(testSubject,testActivity,testMeasurement)

#------------------------------------------------------------------------------
# Get the files for train data and combine data files together so that there is
# a single file for test data with the type of data (train), subject, activityLabel
#and measurements for each of the variables.  Also certain column names will be 
# modified to make them more readable and understandable.
#------------------------------------------------------------------------------
trainActivity<-read.table(trainActivityFilePath, header = FALSE)
names(trainActivity)<-c("activityIndex")
trainSubject <- read.table(trainSubjectPath, header = FALSE)
names(trainSubject)<- c("subject")
trainMeasurement<-read.table(trainMeasurementFilePath, header = FALSE)
names(trainMeasurement)<-features$V2

# Combine Suject, Activity and Measurement into train data
trainData<-cbind(trainSubject,trainActivity,trainMeasurement)

#------------------------------------------------------------------------------
# Bring the test and train data together, get the mean and standard deviation 
# data and "tidy" up the data.
#------------------------------------------------------------------------------
humanActivity<-data.table(rbind(testData,trainData))

# Create a new data frame with only data from columns with mean() or std() are in
# their names.
cn<-names(humanActivity)
cn<-grep("mean\\(\\)|std\\(\\)",cn, value=TRUE)
fHumanActivity<-humanActivity[ , c("subject","activityIndex",cn), with=FALSE]

# Get rid  "-", "()" in column names
names(fHumanActivity)<-gsub("-mean\\(\\)", "mean",names(fHumanActivity))
names(fHumanActivity)<-gsub("-std\\(\\)", "standarddeviation",names(fHumanActivity))
names(fHumanActivity)<-gsub("-[xX]", "x",names(fHumanActivity))
names(fHumanActivity)<-gsub("-[yY]", "y",names(fHumanActivity))
names(fHumanActivity)<-gsub("-[zZ]", "z",names(fHumanActivity))

# Making subject and activityIndex factors
fHumanActivity$subject<- as.factor(fHumanActivity$subject)
fHumanActivity$activityIndex<- as.factor(fHumanActivity$activityIndex)

# Adding the actiivtyName based on the activityIndex from the activityLabels
# data frame.
fHumanActivity<-merge(activityLabels,fHumanActivity, by="activityIndex", all=TRUE)

# Making all column names lower case and removing the activityindex column since
# it is superfluous
names(fHumanActivity)<-tolower(names(fHumanActivity))
fHumanActivity[,"activityindex"]<- list(NULL) 

#------------------------------------------------------------------------------
# Creating a data frame with melt function where id columns are subject and
# activityname and the other columns names become the "variable" column and
# the associated measurement is placed in the "value" column.
#------------------------------------------------------------------------------
fHumanActivity<-melt(data=fHumanActivity,id=c("subject","activityname"))
#------------------------------------------------------------------------------
# Using the dcast function reshape the data with subject and activity dimension
# is grouped together and each unique value in the variable column becomes a
# column and then at the same time take the mean of each column based on subject
# and activityname. Because of the way the melt and dcast work some column
# names needed cleaning up.
#------------------------------------------------------------------------------
meanHumanActivity<-dcast(fHumanActivity, subject+activityname ~ variable,
                         fun.aggregate = mean)
names(meanHumanActivity)<-gsub("bodybody", "body",names(meanHumanActivity))

# write the data frame out to a file named tidyData.csv
write.table(x = meanHumanActivity, file = "tidyData.txt", quote = FALSE)
