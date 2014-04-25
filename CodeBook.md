Getting and cleaning Data : Assignment - CODEBOOK
==================================================
```
INDEX
-----
1. DATA PROCESSING 
2. TIDY DATA
3. EXPLAINING VARIABLE NAMES (in the final TIDY DATA)
4. REFERENCES

### 1. DATA PROCESSING

```{r}
getwd()
```
```
The UCI HAR dataset was first downloaded into the working directory.
The data set has a number of text files as enumerated in the 'READme.md'.**[1]**
The original data had been subset into test (30% data) and train (70% data) subgroups. The original data had variables obtained by processing the acceleration, gyroscope measurements using time and frequency domain and stored as various text files (as listed below) [2].

For the purpose of this project, the following files have been referred from the UCI HAR Dataset **[2]**:
* 'README.txt'
* 'features_info.txt': Shows information about the variables used on the feature vector.
* 'features.txt': List of all features.
* 'activity_labels.txt': Links the class labels with their activity name.
* 'train/X_train.txt': Training set.
* 'train/y_train.txt': Training labels.
* 'test/X_test.txt': Test set.
* 'test/y_test.txt': Test labels.
```
```{r}
# Reading Files
sub.test<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/test/subject_test.txt")
x.test<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/test/X_test.txt")
y.test<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/test/y_test.txt")
sub.train<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/train/subject_train.txt")
x.train<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/train/X_train.txt")
y.train<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/train/y_train.txt")
```
```
After reading the files, we made the test and the train data frames. 
```
```{r}
# Making 'test' and 'train' data frames
test<-data.frame(cbind(sub.test,y.test,x.test))
train<-data.frame(cbind(sub.train,y.train,x.train))
```
```
The 'test' and 'train' data frames were then merged into a single data frame 'merged'
```
```{r}
merged<-data.frame(rbind(test,train))
```
```
This 'merged' dataset has 10299 rows and 563 columns (variables), but does not have proper descriptive names for the variables. Next we read the variable names from 'features.txt' file **[2]** and we append the variables 'subject' and 'activity'.
```
```{r}
# Reading the variable names from 'features.txt' file 
# and storing names into a vector 'c.name.mod'
c.name<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/features.txt")
c.name.mod<-as.vector(c.name[,2])
# Incorporating the labels "subject" and "activity
c.name.mod1<-append(c.name.mod,"subject",after=FALSE)
c.name.mod2<-append(c.name.mod1,"activity",after=1)
# Appropriately labelling the data set with descriptive activity names
colnames(merged)<-c.name.mod2
```
```
Next for the purpose of this project, we are required only to extract those cases that include ** measurements on the mean and standard deviation for each measurement **. So we subset the merged dataset suitably and also include the variables 'subject' and 'activity'.
```
```{r}
# Extracting the measurements on 'mean()' and 'standard deviation()' for each measurement
# into a modified data frame 'merged.mn.sd'
mn<-grep("mean()",colnames(merged),fixed=TRUE)
sd<-grep("std()",colnames(merged),fixed=TRUE)
mn.sd<-c(mn,sd)
merged.mn.sd<-merged[,c(1,2,mn.sd)]
```
```
Next we clean the names used to name the variables in the data set to make the labels more readable.
```
```{r}
names(merged.mn.sd)<-gsub("-",".",names(merged.mn.sd))
names(merged.mn.sd)<-gsub("()","",names(merged.mn.sd),fixed=TRUE)
```
```
Next we explore essential features (e.g names of variables, dimensions of data frame,checking for NA values).
```
```{r}
names(merged.mn.sd)
sum(is.na(merged.mn.sd))
dim(merged.mn.sd)
```
```
### 2. TIDY DATA

Then we create a second independent tidy dataset (ordered by 'subject' and 'activity' 
labels),with the average of each variable for each activity and each subject.       
Then ** we Use descriptive activity names to name the activities in the data set ** and
** appropriately labels the data set with descriptive activity names ** i.e. we have assigned the descriptive factors for activity labels (as in the file "activity_labels.txt") in place of the earlier given numeric factors.
We then study this dataset (e.g names, dimensions).
This ** tidy data ** set has been ordered by subject and activity; has 
** 180 rows (i.e. cases) ** and 
** 68 columns (i.e. variables) **
```
```{r}
# Creates a second, independent tidy dataset 'tidydata' 
# (ordered by "subject" and "activity")
# with the average of each variable for each activity and each subject.
install.packages("plyr")
library(plyr)
tidydata<-ddply(merged.mn.sd,.(subject,activity),numcolwise(mean))
# Inserting the activity labels in place of the numerical factor levels in the tidy dataset 
# to improve the readability of the activity levels the data frame 'tidy.data' 
tidydata[,2]<-mapvalues(tidydata[,2],from = c("1","2","3","4","5","6"),
  to = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
# Exploring features of the tidy dataset named 'tidydata'
# e.g. dimensions, names, the subject and activity columns, head
dim(tidydata)
names(tidydata)
tidydata[,1:2]
head(tidydata)
```
```
Next we saved this tidy data set as **'tidydata.txt'**
Then we get the output tidy data set above as the output of the "run_analysis.R" using the "read.table()" command
```
```{r}
write.table(tidydata, file="./tidydata.txt", sep="\t", row.names=FALSE)
read.table("./tidydata.txt")
```
```
### 3. EXPLAINING VARIABLE NAMES (in the final TIDY DATA)

The experiments were conducted on **30 subjects** 
pertaining to **6 activities** :
1.WALKING, 
2.WALKING_UPSTAIRS, 
3.WALKING_DOWNSTAIRS, 
4.SITTING, 
5.STANDING, 
6.LAYING
 
"The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
**(Note the 't' to indicate the time domain and 'f' the frequency domain signals)**. 
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
tBodyAcc-XYZ          (3)
tGravityAcc-XYZ       (3)
tBodyAccJerk-XYZ      (3)
tBodyGyro-XYZ         (3)
tBodyGyroJerk-XYZ     (3)
tBodyAccMag           (1)
tGravityAccMag        (1)
tBodyAccJerkMag       (1)
tBodyGyroMag          (1)
tBodyGyroJerkMag      (1)
fBodyAcc-XYZ          (3)
fBodyAccJerk-XYZ      (3)
fBodyGyro-XYZ         (3)
fBodyAccMag           (1)
fBodyAccJerkMag       (1)
fBodyGyroMag          (1)
fBodyGyroJerkMag      (1)

The set of variables that were estimated from these signals are: 
mean(): Mean value
std(): Standard deviation" **[2]**
This has been explained as in the UCI HAR dataset "features_info.txt" file. **[2]**
i.e. a total of 68 variables 
= 1 (for subject) + 1 (for activity) + 33 (for mean) + 33 (for standard deviation)

The names of  variables of the tidy data set have been enlisted below :

 [1] "subject"                   "activity"                  "tBodyAcc.mean.X"          
 [4] "tBodyAcc.mean.Y"           "tBodyAcc.mean.Z"           "tGravityAcc.mean.X"       
 [7] "tGravityAcc.mean.Y"        "tGravityAcc.mean.Z"        "tBodyAccJerk.mean.X"      
[10] "tBodyAccJerk.mean.Y"       "tBodyAccJerk.mean.Z"       "tBodyGyro.mean.X"         
[13] "tBodyGyro.mean.Y"          "tBodyGyro.mean.Z"          "tBodyGyroJerk.mean.X"     
[16] "tBodyGyroJerk.mean.Y"      "tBodyGyroJerk.mean.Z"      "tBodyAccMag.mean"         
[19] "tGravityAccMag.mean"       "tBodyAccJerkMag.mean"      "tBodyGyroMag.mean"        
[22] "tBodyGyroJerkMag.mean"     "fBodyAcc.mean.X"           "fBodyAcc.mean.Y"          
[25] "fBodyAcc.mean.Z"           "fBodyAccJerk.mean.X"       "fBodyAccJerk.mean.Y"      
[28] "fBodyAccJerk.mean.Z"       "fBodyGyro.mean.X"          "fBodyGyro.mean.Y"         
[31] "fBodyGyro.mean.Z"          "fBodyAccMag.mean"          "fBodyBodyAccJerkMag.mean" 
[34] "fBodyBodyGyroMag.mean"     "fBodyBodyGyroJerkMag.mean" "tBodyAcc.std.X"           
[37] "tBodyAcc.std.Y"            "tBodyAcc.std.Z"            "tGravityAcc.std.X"        
[40] "tGravityAcc.std.Y"         "tGravityAcc.std.Z"         "tBodyAccJerk.std.X"       
[43] "tBodyAccJerk.std.Y"        "tBodyAccJerk.std.Z"        "tBodyGyro.std.X"          
[46] "tBodyGyro.std.Y"           "tBodyGyro.std.Z"           "tBodyGyroJerk.std.X"      
[49] "tBodyGyroJerk.std.Y"       "tBodyGyroJerk.std.Z"       "tBodyAccMag.std"          
[52] "tGravityAccMag.std"        "tBodyAccJerkMag.std"       "tBodyGyroMag.std"         
[55] "tBodyGyroJerkMag.std"      "fBodyAcc.std.X"            "fBodyAcc.std.Y"           
[58] "fBodyAcc.std.Z"            "fBodyAccJerk.std.X"        "fBodyAccJerk.std.Y"       
[61] "fBodyAccJerk.std.Z"        "fBodyGyro.std.X"           "fBodyGyro.std.Y"          
[64] "fBodyGyro.std.Z"           "fBodyAccMag.std"           "fBodyBodyAccJerkMag.std"  
[67] "fBodyBodyGyroMag.std"      "fBodyBodyGyroJerkMag.std" 


### 4. REFERENCES :
1. 'READme.md' : shared with this project
2. UCI HAR Dataset : (Coursera link)
   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
```
