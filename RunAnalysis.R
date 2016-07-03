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
    write.table(all_data,file = "Tidy_data.txt")
}