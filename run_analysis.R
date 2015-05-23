

library(data.table)
library(dplyr)

#Loading data:
variables_names = read.table("UCI HAR Dataset/features.txt")
activity_names = read.table("UCI HAR Dataset/activity_labels.txt")

#reading training data:
train_person = read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_activity = read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
train_variables = read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#we check there is the same number of rows in the three data sets:
nrow(train_person) == nrow(train_activity)
nrow(train_person) == nrow(train_variables)

#reading testing data:
test_person = read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_activity = read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
test_variables = read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#we check there is the same number of rows in the three data sets:
nrow(test_person) == nrow(test_activity)
nrow(test_person) == nrow(test_variables)


###########################################################################################
# 1 - Merges the training and the test sets to create one data set.
###########################################################################################

#First we merge the couples of traning and testing data in three data sets, using the function rbind
person = rbind(train_person, test_person)
colnames(person) = "PERSON"
activity = rbind(train_activity, test_activity)
colnames(activity) = "ACTIVITY_CODE"
variables = rbind(train_variables, test_variables)

#Now we can use the variables_name to put these names in the variables data.frame
#Firs doing this we check the same length of the respective dataframes
ncol(variables) == nrow(variables_names)
names(variables) = t(variables_names[2])

#Finally we merge de data in the same data.frame called data using cbind function:
data= cbind(person, activity, variables)


###########################################################################################
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
###########################################################################################

#First we create a variable with the names of variables that contains "std" or "mean"
extract_names = grep("*mean*|*std*|*Mean*", names(data) )

#Finally we extract the data from the data.frame data creating a new data.frame
extract_data = data[,c(1:2, extract_names)]


###########################################################################################
# 3 - Uses descriptive activity names to name the activities in the data set.
###########################################################################################

#we will use the activity names table to connect the activity names with the extract_data table
colnames(activity_names)= c("ACTIVITY_CODE", "ACTIVITY_NAME")

#we do a left-join of the extract_data table using the activity_names table and the ACTIVITY_CODE variable
extract_data2 = merge(x = extract_data, y = activity_names, by="ACTIVITY_CODE", all.x=TRUE)
# Now the extract_data2 data.frame contains also a new variable with the ACTIVITY_NAME:
head(extract_data2$ACTIVITY_NAME)


###########################################################################################
# 4 - Appropriately labels the data set with descriptive variable names.
###########################################################################################

#we have done this during the steps before. These are the names of the variables in our data.frame:
names(extract_data2)


###########################################################################################
# 5 - From the data set in step 4, creates a second, independent tidy data set with the average of 
#     each variable for each activity and each subject.
###########################################################################################

#For doing this we need a new data set without the ACTIVITY_CODE variable:
extract_data3 = extract_data2[, -1]

#For doing this, we have to change the variables class. We need the ACTIVITY_NAME and PERSON 
#as factor variables and the data.set as a data.table
class(extract_data3$ACTIVITY_NAME)
extract_data3$PERSON = as.factor(extract_data3$PERSON)
extract_data3 = data.table(extract_data3)

#Finally we create the tidy_data with mean for each PERSON and ACTIVITY_NAME.
tidy_data = aggregate(. ~PERSON + ACTIVITY_NAME, extract_data3, mean)

#we order the data.table by the PERSON and ACTIVITY_NAME variables:
tidy_data = tidy_data[ order(tidy_data$PERSON, tidy_data$ACTIVITY_NAME), ]

#we write the .txt file with the final data.table
write.table(tidy_data, file = "tidy_data_set.txt", row.names=FALSE)

