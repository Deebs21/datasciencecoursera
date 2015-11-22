#read in x files for test and train, then merge
x_test <- read.table("./Data/UCI HAR Dataset/test/X_test.txt", header = FALSE)
x_train <- read.table("./Data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
x_vals <- rbind(x_test,x_train)
#read in subjects for test and train, then merge
test_subjects <- read.table("./Data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
train_subjects <- read.table("./Data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subjects <- rbind(test_subjects,train_subjects)
#read in y files for test and train, then merge
y_test <- read.table("./Data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
y_train <- read.table("./Data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
y_vals <- rbind(y_test,y_train)
#read in feature names
features <- read.table("./Data/UCI HAR Dataset/features.txt", header = FALSE)
#identify only those feature names that contain mean or standard deviation
usable_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
#limit to only those feature names identify as having mean or standard deviation
x_vals <- x_vals[,usable_features]
#update columns names to be more user friendly
names(x_vals) <- features[usable_features,2]
names(x_vals) <- gsub("\\(|\\)","",names(x_vals))
names(x_vals) <- tolower(names(x_vals))
#read in activity names
activities <- read.table("./Data/UCI HAR Dataset/activity_labels.txt", header = FALSE)
#update values to their respective activity name
y_vals[,1] = activities[y_vals[,1],2]
#update column names to be more meaningful
names(y_vals) <- "activity"
names(subjects) <- "subject"
#merge columns together to create tidy dataset
tidyData <- cbind(subjects,y_vals,x_vals)
#output result to text file
write.table(tidyData, "./course_project_tidy_data.txt")
#identify the unique subjects
uniqueSubjects = unique(subjects)[,1]
#count the number of unique subjects
numSubjects = length(unique(subjects)[,1])
#count the number of activities
numActivities = length(activities[,1])
#count the numberf of columns in the tidy data set
numCols = dim(tidyData)[2]
#set the calc to be used to determine the mean
result = tidyData[1:(numSubjects*numActivities), ]
row = 1
#write three columns for subject, activity, and mean of the feature
for (s in 1:numSubjects) {
      for (a in 1:numActivities) {
            result[row, 1] = uniqueSubjects[s]
            result[row, 2] = activities[a, 2]
            tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a, 2], ]
            result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
            row = row+1
      }
}
write.table(result, "tidy_data_with_averages.txt")