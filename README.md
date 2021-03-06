---
title: "README.md"
author: "Amirrudin"
date: "Sunday, September 21, 2014"
output: html_document
---

<br /><h4>Important steps to do before running the run_analysis.R:</h4>

1. Download and unzip the file from the source
2. Ensure files are unzip to a folder named 'UCI_HAR_Dataset' 
3. Set working directory to point to folder 'UCI_HAR_Dataset'
4. Install package 'dplyr' using command: install.packages("dplyr") 
5. load package 'dplyr' using command: library(dplyr)

<br /><h4>Start of script</h4>

Program start by clearing existing variables in the environment

```{r}
rm(list=ls())
```

Set working directory containing unzipped data

```{r}
setwd("D:\\R\\work_directory\\UCI_HAR_Dataset")
```


<br /><h4>Merge the training and the test sets to create one data set.</h4>

Read training related data 

```{r}
features = read.table(".\\features.txt")
activity_data = read.table(".\\activity_labels.txt")
subject_train = read.table(".\\train\\subject_train.txt")
x_train = read.table(".\\train\\x_train.txt")
y_train = read.table(".\\train\\y_train.txt")
```

Read test related data 

```{r}
subject_test = read.table(".\\test\\subject_test.txt")
x_test = read.table(".\\test\\x_test.txt")
y_test = read.table(".\\test\\y_test.txt")
```

Set column names to training related data sets 

```{r}
colnames(activity_data) = c("activity_id","activity_name")
colnames(subject_train) = "subject_id"
```

For x_train dataset, assign the data from 2nd column of features to it

```{r}
colnames(x_train) = features[,2]
colnames(y_train) = "activity_id"
```

Set column names to test related data sets

```{r}
colnames(subject_test) = "subject_id"
```

For x_test dataset, assign the data from 2nd column of features to it

```{r}
colnames(x_test) = features[,2]
colnames(y_test) = "activity_id"
```

Combine train related data using 'cbind(y_train,subject_train,x_train)' and column bind

```{r}
train_test_master_data = rbind(cbind(y_train,subject_train,x_train),cbind(y_test,subject_test,x_test))
```

Create a vector for the column names from the train_test_master_data, which will be used to select the desired mean() & stddev() columns

```{r}
colNames = colnames(train_test_master_data)
```

<br /><h4>Extract only the measurements on the mean and standard deviation for each measurement.</h4>

Create placeholder for index of desired columns only

```{r}
index_of_desired_columns = 
(
grepl("activity_id",colNames)  | # only activity_id
grepl("subject_id",colNames)   | # only subject_id
grepl("-mean\\(\\)$",colNames) | # ends with -mean() only
grepl("-std\\(\\)$",colNames)    # ends with -std() only
)
```

Get desired data from the desired columns indicated with true flag

```{r}
train_test_master_data = train_test_master_data[index_of_desired_columns==TRUE]
```

<br />
<h4>Use descriptive activity names to name the activities in the data set.</h4>
<h4>Appropriately label the data set with descriptive activity names.</h4>

Get column names

```{r}
column_names_of_train_test_master_data = colnames(train_test_master_data)
```

Assign friendlier names to columns so mean and standard deviation columns are easily identified

```{r}
for (i in 1:length(column_names_of_train_test_master_data))
{
column_names_of_train_test_master_data[i] = gsub("\\()","",column_names_of_train_test_master_data[i])
column_names_of_train_test_master_data[i] = gsub("-std$","_Standard_Deviation",column_names_of_train_test_master_data[i])
column_names_of_train_test_master_data[i] = gsub("-mean","_Mean",column_names_of_train_test_master_data[i])
column_names_of_train_test_master_data[i] = gsub("tBody","timeBody",column_names_of_train_test_master_data[i])
column_names_of_train_test_master_data[i] = gsub("tGravity","timeGravity",column_names_of_train_test_master_data[i])
column_names_of_train_test_master_data[i] = gsub("fBody","frequencyBody",column_names_of_train_test_master_data[i])
}
```

Rename columns with new names

```{r}
colnames(train_test_master_data) = column_names_of_train_test_master_data
```

<br /><h4>Create a second, independent tidy data set with the average of each variable for each activity and each subject.</h4>

Load dplyr package

```{r}
library(dplyr)
```

Convert to df for dplyr package 

```{r}
train_test_master_data_df = tbl_df(train_test_master_data)
```

Order data set with the average of each variable for each activity and each subject 

```{r}
ordered_data <- train_test_master_data_df %.% 
group_by(activity_id, subject_id)%.%	
summarize(
timeBodyAccMag_Mean=mean(timeBodyAccMag_Mean),
timeBodyAccMag_Standard_Deviation=mean(timeBodyAccMag_Standard_Deviation),
timeGravityAccMag_Mean=mean(timeGravityAccMag_Mean),
timeGravityAccMag_Standard_Deviation=mean(timeGravityAccMag_Standard_Deviation),
timeBodyAccJerkMag_Mean=mean(timeBodyAccJerkMag_Mean),
timeBodyAccJerkMag_Standard_Deviation=mean(timeBodyAccJerkMag_Standard_Deviation),
timeBodyGyroMag_Mean=mean(timeBodyGyroMag_Mean),
timeBodyGyroMag_Standard_Deviation=mean(timeBodyGyroMag_Standard_Deviation),
timeBodyGyroJerkMag_Mean=mean(timeBodyGyroJerkMag_Mean),
timeBodyGyroJerkMag_Standard_Deviation=mean(timeBodyGyroJerkMag_Standard_Deviation),
frequencyBodyAccMag_Mean=mean(frequencyBodyAccMag_Mean),
frequencyBodyAccMag_Standard_Deviation=mean(frequencyBodyAccMag_Standard_Deviation),
frequencyBodyBodyAccJerkMag_Mean=mean(frequencyBodyBodyAccJerkMag_Mean),
frequencyBodyBodyAccJerkMag_Standard_Deviation=mean(frequencyBodyBodyAccJerkMag_Standard_Deviation),
frequencyBodyBodyGyroMag_Mean=mean(frequencyBodyBodyGyroMag_Mean),
frequencyBodyBodyGyroMag_Standard_Deviation=mean(frequencyBodyBodyGyroMag_Standard_Deviation),
frequencyBodyBodyGyroJerkMag_Mean=mean(frequencyBodyBodyGyroJerkMag_Mean),
frequencyBodyBodyGyroJerkMag_Standard_Deviation=mean(frequencyBodyBodyGyroJerkMag_Standard_Deviation)
)		
```

Merge data by activity_id

```{r}
ordered_data = merge(ordered_data,activity_data,by='activity_id',all.x=TRUE)
```

Move activity name to 3rd column

```{r}
tidy_data = c(ordered_data[1:2],ordered_data[21],ordered_data[3:20])
```

Create file tidy_data.txt

```{r}
write.table(tidy_data, ".\\tidy_data.txt",row.names=TRUE,sep="\t")
```

