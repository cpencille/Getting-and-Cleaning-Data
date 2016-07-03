---
title: "Getting and Cleaning Data Assignment"
author: "Chris Pencille"
date: "July 2, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# About
This is the assignment for the Getting and Cleaning Data course in Coursera's Data Science Specialization. The objective is to take several data sets and merge them into one tidy data set.



## R Info and Libraries needed
library(plyr) - needed for join()

R version 3.2.5 (2016-04-14)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

attached base packages:
stats     graphics  grDevices   utils     datasets  methods base     

other attached packages:
plyr_1.8.4     reshape2_1.4.1   markdown_0.7.7

#Downloaded Data
Data can be found at the following link: [link](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

The following files will be needed for the analysis and should be placed into the working directory to use:
1. activity_labels.txt
2. features.txt
3. subject_test.txt
4. subject_train.txt
5. X_test.txt
6. X_train.txt
7. y_test.txt
8. y_train.txt

#Creating the Tidy Data Set
The analysis is ran in a single function for ease of use. The summary steps include:

1. Reading in all data from above
2. Combining all subjects data
3. Combining all features data
4. combine all activities data
5. Subset features data to only include mean and standard deviation columns
6. Re-order data and move columns around to create a more readable data set
7. Output Tidy Data set to a txt file

```{r eval=FALSE}
run_analysis<- function(){
    
    #Read in all the text files needed for the analysis
    #read in subject IDs
    subject_test<-read.table("Subject_test.txt",col.names=c("Subject"))
    subject_train<-read.table("subject_train.txt",col.names = c("Subject"))
    
    #read in feature list and activities labels
    feature_list<-read.table("features.txt",col.names = c("index","feature_labels"))
    activity_labels<-read.table("activity_labels.txt",sep = " ",col.names = c("activityLabel","Activity"))
    
    #read in features data
    features_test<-read.table("X_test.txt")
    features_train<-read.table("X_train.txt")
    
    #read in activities data
    activities_test<-read.table("y_test.txt")
    activities_train<-read.table("y_train.txt")
    
    ##Combine subject IDs into one data frame
    all_subject<-rbind(subject_test,subject_train)
    
    ##Combine all features data into one data frame
    features_data<-rbind(features_test,features_train)
    
    ##Combine all activities into one data frame and rename the "V1" column to "Activity"
    all_activities<-rbind(activities_test,activities_train)
    colnames(all_activities)<-"activityLabel"
    
    ##Convert activity values into names using the activity labels data frame
    all_activities<-join(all_activities,activity_labels,by="activityLabel",type="left")
    ##Drop the numbers from the data frame
    all_activities$activityLabel <- NULL
        
    #Create a 1 dimensional character vector containing the feature labels
    feature_labels<-feature_list$feature_labels
    
    ##Rename columns in the features data to prepare it to be subsetted
    colnames(features_data)<-feature_labels
    
    ##Create a logical vector for the columns containing mean and std 
    features_subset<-grepl('mean\\(\\)|std\\(\\)',feature_labels)
    
    ##Create a vector with only features with mean and std in their name
    feature_list<-as.character(feature_labels[features_subset])
    
    ##Subset to columns containing only mean() and std() from the data set
    features_data<-features_data[,features_subset]
    
    ##Combine activities, Subjects, and Features data into one data frame
    all_data<-cbind(features_data,all_activities,all_subject)
    
    ##Reorder by subject and Activity
    all_data<-all_data[order(all_data$Subject,all_data$Activity),]
    
    #Move the subject to be the first column and Activity to the second column
    all_data<-all_data[,c(68,67,1:66)]
    
    ##create the final output data
    write.table(all_data,file = "Tidy_data.txt",row.names = FALSE)
}
```