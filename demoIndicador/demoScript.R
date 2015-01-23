# Data Cleaning Project
###########################
################################################
## Working with the test data set
################################################

# reading subject_test data (subjects)
df_subject_test <- read.table("./project/UCI HAR Dataset/test/subject_test.txt")

# reading X_test (data)
df_X_test <- read.table("./project/UCI HAR Dataset/test/X_test.txt")

# reading y_test test data (activity labels)
df_labels_test <- read.table("./project/UCI HAR Dataset/test/y_test.txt")

# changing the column name from "V1" to "activity" in df_labels_test before merge
colnames(df_labels_test)[1] <- "activity"

# changing the column name from "V1" to "subject" in df_subject_test para "subject" before merge
colnames(df_subject_test)[1] <- "subject"

# merging: df_subject_test, df_labels_test and df_X_test into df_s_X_test. 
df_l_s_X_test <- cbind(df_subject_test,df_labels_test,df_X_test)

################################################
## Working with the train data set
################################################
# reading subject_train data (subjects)
df_subject_train <- read.table("./project/UCI HAR Dataset/train/subject_train.txt")

# reading X_train (data)
df_X_train <- read.table("./project/UCI HAR Dataset/train/X_train.txt")

# reading y_train train data (activity labels)
df_labels_train <- read.table("./project/UCI HAR Dataset/train/y_train.txt")

# changing the column name from "V1" to "activity" in df_labels_train before merge
colnames(df_labels_train)[1] <- "activity"

# changing the column name from "V1" to "subject" in df_subject_train para "subject" before merge
colnames(df_subject_train)[1] <- "subject"

# merging: df_subject_train, df_labels_train e df_X_train em df_l_s_X_train. Obs. cbind muito mais rápido que merge!!
df_l_s_X_train <- cbind(df_subject_train,df_labels_train,df_X_train)

# mergin both dataset (test e train) into a single one
df_total <- rbind (df_l_s_X_test,df_l_s_X_train)


# Extracting only the mean e std columns (those that apear into features.txt file)
library(dplyr) # using dplyr package
df_mean_std <- select(df_total,subject,activity,V1:V6, V41:V46,V81:V86,V121:V126,V161:V166,V201:V202,
                      V214:V215,V227:V228,V240:V241,V253:V254,V266:V271,V345:V350,
                      V424:V429,V503:V504,V516:V517,V529:V530,V542:V543)


# clearing the not longer needed datasets before proceed...
# this is needed to free memory when dealing with big datasets
rm(df_l_s_X_test,df_l_s_X_train,df_X_test, df_X_train, df_total)


# changing the numeric values of activity variable to descriptive names
distinct(select(df_mean_std,activity))  # making sure the data.frame has only the valid activity activity numbers
df_mean_std$activity <- as.character(df_mean_std$activity) # transforming activity from integer type to character type
df_mean_std$activity[df_mean_std$activity == 1] <- "WALKING"
df_mean_std$activity[df_mean_std$activity == 2] <- "WALKING_UPSTAIRS"
df_mean_std$activity[df_mean_std$activity == 3] <- "WALKING_DOWNSTAIRS"
df_mean_std$activity[df_mean_std$activity == 4] <- "SITTING"
df_mean_std$activity[df_mean_std$activity == 5] <- "STANDING"
df_mean_std$activity[df_mean_std$activity == 6] <- "LAYING"

# creating the consolidated dataframe, grouped the mean values by label activity and subject
df_grouped <- group_by(df_mean_std, activity, subject)
df_tidy <- summarise(df_grouped,mean(V1), mean(V2),mean(V3), mean(V4),mean(V5), mean(V6),
                 mean(V41), mean(V42),mean(V43), mean(V44),mean(V45), mean(V46),
                 mean(V81), mean(V82),mean(V83), mean(V84),mean(V85), mean(V86),
                 mean(V121), mean(V122),mean(V123), mean(V124),mean(V125), mean(V126),
                 mean(V161), mean(V162),mean(V163), mean(V164),mean(V165), mean(V166),
                 mean(V201), mean(V202),
                 mean(V214), mean(V215),
                 mean(V227), mean(V228),
                 mean(V240), mean(V241),
                 mean(V253), mean(V254),
                 mean(V266), mean(V267),mean(V268), mean(V269),mean(V270), mean(V271),
                 mean(V345), mean(V346),mean(V347), mean(V348),mean(V349), mean(V350),
                 mean(V424), mean(V425),mean(V426), mean(V427),mean(V428), mean(V429),
                 mean(V503), mean(V504),
                 mean(V516), mean(V517),
                 mean(V529), mean(V530),
                 mean(V542), mean(V543)
                 )

# creating descriptive names for variables in the range V1:V6
names(df_tidy)[names(df_tidy) == 'mean(V1)'] <- "BodyAcceleration_MeanTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V2)'] <- "BodyAcceleration_MeanTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V3)'] <- "BodyAcceleration_MeanTimeDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V4)'] <- "BodyAcceleration_StddevTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V5)'] <- "BodyAcceleration_StddevTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V6)'] <- "BodyAcceleration_StddevTimeDirZ"

# creating descriptive names for variables in the range V41:V46
names(df_tidy)[names(df_tidy) == 'mean(V41)'] <- "GravityAcceleration_MeanTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V42)'] <- "GravityAcceleration_MeanTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V43)'] <- "GravityAcceleration_MeanTimeDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V44)'] <- "GravityAcceleration_StddevTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V45)'] <- "GravityAcceleration_StddevTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V46)'] <- "GravityAcceleration_StddevTimeDirZ"

# creating descriptive names for variables in the range V81:V86
names(df_tidy)[names(df_tidy) == 'mean(V81)'] <- "BodyAccelerationJerk_MeanTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V82)'] <- "BodyAccelerationJerk_MeanTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V83)'] <- "BodyAccelerationJerk_MeanTimeDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V84)'] <- "BodyAccelerationJerk_StddevTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V85)'] <- "BodyAccelerationJerk_StddevTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V86)'] <- "BodyAccelerationJerk_StddevTimeDirZ"

# creating descriptive names for variables in the range V121:V126
names(df_tidy)[names(df_tidy) == 'mean(V121)'] <- "BodyGyroscopic_MeanTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V122)'] <- "BodyGyroscopic_MeanTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V123)'] <- "BodyGyroscopic_MeanTimeDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V124)'] <- "BodyGyroscopic_StddevTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V125)'] <- "BodyGyroscopic_StddevTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V126)'] <- "BodyGyroscopic_StddevTimeDirZ"

# creating descriptive names for variables in the range V161:V166
names(df_tidy)[names(df_tidy) == 'mean(V161)'] <- "BodyGyroscopicJerk_MeanTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V162)'] <- "BodyGyroscopicJerk_MeanTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V163)'] <- "BodyGyroscopicJerk_MeanTimeDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V164)'] <- "BodyGyroscopicJerk_StddevTimeDirX"
names(df_tidy)[names(df_tidy) == 'mean(V165)'] <- "BodyGyroscopicJerk_StddevTimeDirY"
names(df_tidy)[names(df_tidy) == 'mean(V166)'] <- "BodyGyroscopicJerk_StddevTimeDirZ"

# creating descriptive names for variables in the range V201:V202
names(df_tidy)[names(df_tidy) == 'mean(V201)'] <- "BodyAcceleration_MeanTimeMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V202)'] <- "BodyAcceleration_StddevTimeMagnitude"

# creating descriptive names for variables in the range V214:V215
names(df_tidy)[names(df_tidy) == 'mean(V214)'] <- "GravityAcceleration_MeanTimeMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V215)'] <- "GravityAcceleration_StddevTimeMagnitude"

# creating descriptive names for variables in the range V227:V228
names(df_tidy)[names(df_tidy) == 'mean(V227)'] <- "BodyAccelerationJerk_MeanTimeMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V228)'] <- "BodyAccelerationJerk_StddevTimeMagnitude"

# creating descriptive names for variables in the range V240:V241
names(df_tidy)[names(df_tidy) == 'mean(V240)'] <- "BodyGyroscopic_MeanTimeMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V241)'] <- "BodyGyroscopic_StddevTimeMagnitude"

# creating descriptive names for variables in the range V253:V254
names(df_tidy)[names(df_tidy) == 'mean(V253)'] <- "BodyGyroscopicJerk_MeanTimeMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V254)'] <- "BodyGyroscopicJerk_StddevTimeMagnitude"

# creating descriptive names for variables in the range V266:V271
names(df_tidy)[names(df_tidy) == 'mean(V266)'] <- "BodyAcceleration_MeanFfourierDirX"
names(df_tidy)[names(df_tidy) == 'mean(V267)'] <- "BodyAcceleration_MeanFfourierDirY"
names(df_tidy)[names(df_tidy) == 'mean(V268)'] <- "BodyAcceleration_MeanFfourierDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V269)'] <- "BodyAcceleration_StddevFfourierDirX"
names(df_tidy)[names(df_tidy) == 'mean(V270)'] <- "BodyAcceleration_StddevFfourierDirY"
names(df_tidy)[names(df_tidy) == 'mean(V271)'] <- "BodyAcceleration_StddevFfourierDirZ"

# creating descriptive names for variables in the range V345:V350
names(df_tidy)[names(df_tidy) == 'mean(V345)'] <- "BodyAccelerationJerk_MeanFfourierDirX"
names(df_tidy)[names(df_tidy) == 'mean(V346)'] <- "BodyAccelerationJerk_MeanFfourierDirY"
names(df_tidy)[names(df_tidy) == 'mean(V347)'] <- "BodyAccelerationJerk_MeanFfourierDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V348)'] <- "BodyAccelerationJerk_StddevFfourierDirX"
names(df_tidy)[names(df_tidy) == 'mean(V349)'] <- "BodyAccelerationJerk_StddevFfourierDirY"
names(df_tidy)[names(df_tidy) == 'mean(V350)'] <- "BodyAccelerationJerk_StddevFfourierDirZ"

# creating descriptive names for variables in the range V424:V429
names(df_tidy)[names(df_tidy) == 'mean(V424)'] <- "BodyGyroscopic_MeanFfourierDirX"
names(df_tidy)[names(df_tidy) == 'mean(V425)'] <- "BodyGyroscopic_MeanFfourierDirY"
names(df_tidy)[names(df_tidy) == 'mean(V426)'] <- "BodyGyroscopic_MeanFfourierDirZ"
names(df_tidy)[names(df_tidy) == 'mean(V427)'] <- "BodyGyroscopic_StddevFfourierDirX"
names(df_tidy)[names(df_tidy) == 'mean(V428)'] <- "BodyGyroscopic_StddevFfourierDirY"
names(df_tidy)[names(df_tidy) == 'mean(V429)'] <- "BodyGyroscopic_StddevFfourierDirZ"

# creating descriptive names for variables in the range V503:V504
names(df_tidy)[names(df_tidy) == 'mean(V503)'] <- "BodyAcceleration_MeanFfourierMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V504)'] <- "BodyAcceleration_StddevFfourierMagnitude"

# creating descriptive names for variables in the range V516:V517
names(df_tidy)[names(df_tidy) == 'mean(V516)'] <- "BodyAccelerationJerk_MeanFfourierMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V517)'] <- "BodyAccelerationJerk_StddevFfourierMagnitude"

# creating descriptive names for variables in the range V529:V530
names(df_tidy)[names(df_tidy) == 'mean(V529)'] <- "BodyGyroscopic_MeanFfourierMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V530)'] <- "BodyGyroscopic_StddevFfourierMagnitude"

# creating descriptive names for variables in the range V542:V543
names(df_tidy)[names(df_tidy) == 'mean(V542)'] <- "BodyGyroscopicJerk_MeanFfourierMagnitude"
names(df_tidy)[names(df_tidy) == 'mean(V543)'] <- "BodyGyroscopicJerk_StddevFfourierMagnitude"

# write the tidy file
write.table(df_tidy, file = "./data/tidy_project.txt",sep = ",",row.name = FALSE)

# uncomment the code below to test the final file generated. 
# remember that the variable names are not changed in this dataset (V1, V2, etc).
# but as it is in the same order, it is easy to select some values and check against the data file.
# you can import the text file to Excel to make the things easier
# x <- df_mean_std$V2[df_mean_std$activity == "WALKING" & df_mean_std$subject == 14]
# mean(x)


