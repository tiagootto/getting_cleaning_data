
###########################
# Read all data
###########################

trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")


###########################
# Step1
###########################

joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

###########################
# Step2
###########################

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, meanStdIndices]
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinData) <- gsub("mean", "Mean", names(joinData)) 
names(joinData) <- gsub("std", "Std", names(joinData)) 
names(joinData) <- gsub("-", "", names(joinData)) 

###########################
# Step3
###########################

activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

###########################
# Step4
###########################

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
#write.table(cleanedData, "merged_data.txt") # write the 1st dataset

###########################
# Step5
###########################

finalData <- as.data.frame(matrix(NA,
                                  nrow=nrow(activity)*length(table(joinSubject)),
                                  ncol=ncol(cleanedData))
                           )
colnames(finalData) <- colnames(cleanedData)
row <- 1
for( i in 1:length(table(joinSubject)) ) {
    for(j in 1:nrow(activity)) {
        finalData[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        finalData[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        finalData[row, 3:ncol(cleanedData) ] <- colMeans(cleanedData[bool1&bool2, 3:ncol(cleanedData)])
        row <- row + 1
    }
}
write.table(finalData, "tidy_data.txt")
