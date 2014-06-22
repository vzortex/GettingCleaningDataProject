#Run analysis works on a the HAR dataset obtained from the UCI domain (
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
# It takes the raw data set containing test and train data for Time/Frequency domain
# variables for several observations across multiple subjects and multiple activities
# Specifically, the script does the following:


#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#6. Writes out the clean data set as a space delimited txt file

run_analysis <- function(directory="") {
        
        
        ## Account for trailing "/" in directory
        if(substr(directory,nchar(directory),nchar(directory)) == "/")
                dir_str <- directory

        else if (directory == "") {
                dir_str <- directory
        } else {
                dir_str <- paste(directory, "/", sep="")
        }
          
        ## Create file strings to read
        test_subjects_file   <- paste(dir_str,"test/subject_test.txt",sep="")
        test_activities_file <- paste(dir_str,"test/y_test.txt", sep="")
        test_features_file   <- paste(dir_str,"test/X_test.txt", sep="")
        
        train_subjects_file   <- paste(dir_str,"train/subject_train.txt", sep="")
        train_activities_file <- paste(dir_str,"train/y_train.txt", sep="")
        train_features_file   <- paste(dir_str,"train/X_train.txt", sep="")
        
        features_file        <- paste(dir_str,"features.txt", sep="")
        
        
        ## Read feature names
        features_table  <- read.table(file=features_file)
        features <- as.vector(features_table[[2]])
        
        #Read Test Subjects, Activities and Features and set appropriate column names
        test_subjects   <- read.table(file=test_subjects_file)
        colnames(test_subjects) <- "Subject"
        test_activities <- read.table(file=test_activities_file)
        colnames(test_activities) <- "Activity"
        test_features       <- read.table(file=test_features_file)
        colnames(test_features) <- features
        
        #Create Test data by combining subjects, activities and features data
        test_data <- cbind(test_subjects, test_activities,test_features)
        
        #Read Test Subjects, Activities and Features and set appropriate column names
        train_subjects   <- read.table(file=train_subjects_file)
        colnames(train_subjects) <- "Subject"
        train_activities <- read.table(file=train_activities_file)
        colnames(train_activities) <- "Activity"
        train_features       <- read.table(file=train_features_file)
        colnames(train_features) <- features
        
        #Create Train data by combining subjects, activities and features data
        train_data <- cbind(train_subjects, train_activities,train_features)
        
        #Merge test and train data
        merged_data <- rbind(test_data,train_data)
        
        #Creating columss for first data set with only std deviation and mean measurements
        std_str <- grep("std()",features,ignore.case=TRUE)
        mean_str <- grep("mean()",features,ignore.case=TRUE)
        features_std_mean_id <- c(std_str,mean_str)
        features_std_mean <- features[features_std_mean_id]
        dataset1_columns <- c("Subject", "Activity", features_std_mean)
        
        #Creating first data set with only std deviation and mean measurements
        dataset1 <- merged_data[ , which(names(merged_data) %in% dataset1_columns)]
        
        #Giving Descsriptive activity names
        dataset1$'Activity'[dataset1$'Activity'==1] <- "WALKING"
        dataset1$'Activity'[dataset1$'Activity'==2] <- "WALKING UPSTAIRS"
        dataset1$'Activity'[dataset1$'Activity'==3] <- "WALKING DOWNSTAIRs"
        dataset1$'Activity'[dataset1$'Activity'==4] <- "SITTING"
        dataset1$'Activity'[dataset1$'Activity'==5] <- "STANDING"
        dataset1$'Activity'[dataset1$'Activity'==6] <- "LAYING"
        
        #Giving descriptive names to feature variables
        names(dataset1) <-  sub("std()", "StdDev", names(dataset1) )
        names(dataset1) <-  sub("std()", "Mean", names(dataset1) )
        names(dataset1) <-  sub("tBody", "TimeDomainBody", names(dataset1) )
        names(dataset1) <-  sub("fBody", "FreqDomainBody", names(dataset1) )
        names(dataset1) <-  sub("tGravity", "TimeDomainGravity", names(dataset1) )
        names(dataset1) <-  sub("fGravity", "FreqDomainGravity", names(dataset1) )
        
        #Melting data set to reshape
        meltdataset1 <- melt(dataset1, id.vars=c("Activity", "Subject"))
        
        #Recasting melted data into 2nd data set Averaging all mean and std deviation features across Activities and Subjects
        dataset2 <- dcast(meltdataset1,Activity + Subject ~ variable, mean)
        
        #Writing space delimited clean data set out
        write.table(dataset2, file="UCIHAR_ModDataSet.txt", row.names=FALSE, col.names=TRUE, sep=" ", quote=FALSE)
        
        
}