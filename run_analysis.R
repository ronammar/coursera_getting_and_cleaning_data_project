##### Coursera course project for 'Getting and Cleaning Data'
# Date: Dec/23/2015
# Author: Ron Ammar

# NOTE: At the outset, the objectives of this course project were not clear.
# From what I understand, we are to examine only 3 files within /train and 
# /test of the UCI HAR dataset, skipping the subfolders and their respective
# testing and training data. I would recommend improving the assignment handout
# to be more descriptive.

library(dplyr)
library(stringr)
library(tidyr)


setwd("./UCI HAR Dataset/")


mergeLabelandExtract <- function() {
  # 1) Merges the training and test sets to create one data set.
  # 3) Uses descriptive activity names to name the activities in the data set.
  # 4) Labels the variable names descriptively (simpler to do this upon
  #    data import, rather than after steps 2 and 3).
  # 2) Extracts the mean and standard deviation for each measurement.
  #
  # Returns:
  #   A master list integrating subject ID, type of activity and all
  #   measurements for both training and testing data.
  
  dir.create("merged")
  
  # Get all files with the "test" or "train" suffix
  # We're not interested in the subfolders, so no recursive listing.
  files <- list.files("test", recursive=F)
  testingFiles <- paste("test/", files[str_detect(files, ".+test\\.txt")],
                        sep="")
  files <- list.files("train", recursive=F)
  trainingFiles <- paste("train/", files[str_detect(files, ".+train\\.txt")],
                         sep="")
  
  # Here, I'm making the assumption (based on prior knowledge) that the file
  # number and order in both test and train sets is the same and both have
  # the same prefixes at the same point in the list. We could run more checks
  # with regex in a production version of this script, but the true intention
  # is to only run this analysis once.
  for (i in 1:length(testingFiles)) {
    # Merge training and testing data into a single data set.
    # NOTE: sep="" uses any whitespace, logically equivlent to "\s+"
    te <- read.delim(testingFiles[i], header=F, sep="", colClasses="numeric")
    tr <- read.delim(trainingFiles[i], header=F, sep="", colClasses="numeric")
    merged <- bind_rows(te, tr)
    
    # Extract leading directory names
    filePrefix <- str_match(testingFiles[i], "([^/]+)test\\.txt")[1, 2]
    write.table(merged, paste("merged/", filePrefix, "merged.txt", sep=""),
                row.names=F, col.names=F)
  }
  
  # Create a single data frame combining the 3 sources of data which have now
  # been merged.
  
  # Import activities and use activity labels
  activity <- read.delim("merged/y_merged.txt", col.names="activity",
                         header=F, sep="", colClasses="numeric")
  activityLabels <- read.delim("activity_labels.txt",
                               col.names=c("activity", "ActivityLabels"),
                               header=F, sep="")
  activity <- left_join(activity, activityLabels, by="activity")
  activity <- select(activity, -activity)  # no need for numerical column
  
  
  subjectID <- read.delim("merged/subject_merged.txt", 
                          col.names="SubjectID", header=F, sep="",
                          colClasses="numeric")
  
  # Import features and use them as the header for measurements
  features <- (read.delim("features.txt", header=F, sep="",
                         colClasses="character"))[, 2]  # extract 2nd column
  measurements <- read.delim("merged/X_merged.txt", col.names=features,
                             header=F, sep="", colClasses="numeric")
  
  # Only include mean or stdev features.
  meanOrStdevFeatureIndeces <- str_detect(features, "(mean|std)\\(\\)")
  measurements <- measurements[, meanOrStdevFeatureIndeces]
  
  master <- bind_cols(subjectID, activity, measurements)
  
  return(master)
}


computeMeans <- function(master) {
  # 5) Creates a second independent tidy data set with the average of each
  # variable for each activity and each subject.
  #
  # Output:
  #   Creates new file of table for this analysis.
  
  means <- master %>%
    group_by(SubjectID, ActivityLabels) %>%
    summarise_each(funs(mean)) %>%
    gather("measurement", "mean", -c(SubjectID, ActivityLabels))
  
  write.table(means, "means_tidy.txt", sep="\t", row.names=F)
}


# Run the functions above.
m <- mergeLabelandExtract()
computeMeans(m)









