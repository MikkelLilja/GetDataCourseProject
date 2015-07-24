### Data is downloaded manually and unpacked into the a chosen folder
### Set the directory for the folder:
directory <- "C:/Users/Mikkel.A.Lilja/Desktop/Coursera/data/UCI HAR Dataset"
setwd(directory)

### Loading relevant datasets

x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

activity_labels <- read.table("./activity_labels.txt")
features <- read.table("./features.txt")

### Merging the test and training datasets

x <- rbind(x_train, x_test)
subject <- rbind(subject_train, subject_test)
y <- rbind(y_train, y_test)

### Removing the unwanted measures

measureIndices <- grep("-mean\\(|-std\\(", features[, 2])
x <- x[, measureIndices]

### Changing y from activity ID to correct activity label

#y <- merge(y, activity_labels, by.x = "V1", by.y = "V1", sort = FALSE)$V2
y[,1] <- activity_labels[y[,1], 2]

### Updating names

names(x) <- features[measureIndices, 2]
names(subject) <- "subject"

### Merging it all together to create one dataset

tidyData <- cbind(subject, y, x)
names(tidyData)[2] = "activity"

### Creating the second tidy dataset with the averages 

### Initializing 
avgData <- matrix(NA,30*6,68)
numOfSubjects <- 30
numOfActivities <- 6

### Looping over desired data subsets, calculating the average and storing in
### the matrix

for (sub in 1:numOfSubjects) {
        for (act in 1:numOfActivities) {
                temp <- subset(tidyData, subject == sub & activity == activity_labels[act, 2])
                avgData[numOfActivities*(sub-1) + act, 1:2] <- c(sub, act)
                for (feature in 3:68) {
                        avgData[numOfActivities*(sub-1) + act, feature] = mean(temp[,feature])
                }
                
        }
}

avgDataDone <- as.data.frame(avgData)
names(avgDataDone) <- names(tidyData)

write.table(tidyData, "tidyData.txt")
write.table(avgDataDone, "avgData.txt", row.name = FALSE)

