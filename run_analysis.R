# This script assumes that data is downloaded and extracted in data directory.
# run_analysis.R and data dir both are in same directory.
train_X <- read.table('data/train/X_train.txt')
train_y <- read.table('data/train/y_train.txt')
train_subject <- read.table('data/train/subject_train.txt')

test_X <- read.table('data/test/X_test.txt')
test_y <- read.table('data/test/y_test.txt')
test_subject <- read.table('data/test/subject_test.txt')

# 1. Merges the training and the test sets to create one data set.
train_test_X <- rbind(train_X, test_X)
train_test_y <- rbind(train_y, test_y)
train_test_subject <- rbind(train_subject, test_subject)



# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table('data/features.txt')

# Now lets grep features data frame for indices where we match "mean" or "std"
# if I use this "[Mm]ean|[Ss]td" regex then I get this row matched "561 561 angle(Z,gravityMean)"
# which is not the correct measurement. Need to be careful.
mean_std_indices <- grep("[Mm]ean\\(\\)|[Ss]td\\(\\)", features[, 2])
names_req <- features[mean_std_indices, 2]
# Now lets filter out the columns in train_test_X that matches those in mean_std_indices.
train_test_X <- train_test_X[, mean_std_indices]
names_req <- gsub('[-()]', '', names_req)
names(train_test_X) <- gsub("-", "", names(train_test_X))



# 3. Uses descriptive activity names to name the activities in the data set 
activity_labels <- read.table("data/activity_labels.txt")
activity_levels_names <- activity_labels[train_test_y[, 1], 2]
# lets rename V1 with activity_name 
train_test_y[, 1] = activity_levels_names
names(train_test_y) <- "activity_name"

# 4 Appropriately labels the data set with descriptive variable names.
names(train_test_subject) <- "subject"
cleaned_data <- cbind(train_test_subject, train_test_y, train_test_X)
write.table(cleaned_data, "merged_data.txt")


#5 From the data set in step 4, creates a second, independent tidy data set with
#  the average of each variable for each activity and each subject.
library(reshape2)

merge_data <- rbind(train_X, test_X)
colnames(merge_data) <- c("subject", "activity", names_req)
merge_data$activity <- factor(merge_data$activity, levels = activity_labels[,1], labels = activity_labels[,2])
merge_data$subject <- as.factor(merge_data$subject)
merge_data.melted <- melt(merge_data, id = c("subject", "activity"))
merge_data.mean <- dcast(merge_data.melted, subject+activity ~ variable, mean)
write.table(merge_data.mean, "merged_mean_tidy.txt", row.names = F, quote = F)
