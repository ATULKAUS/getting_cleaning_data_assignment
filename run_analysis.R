##  Getting the working directory

getwd()
[1] "C:/Users/Toshiba/Documents"

## 'UCI HAR Dataset' was downloaded and unzipped into the working directory 

## Reading Files

sub.test<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/test/subject_test.txt")
x.test<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/test/X_test.txt")
y.test<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/test/y_test.txt")
sub.train<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/train/subject_train.txt")
x.train<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/train/X_train.txt")
y.train<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/train/y_train.txt")

## Making 'test' and 'train' data frames

test<-data.frame(cbind(sub.test,y.test,x.test))
train<-data.frame(cbind(sub.train,y.train,x.train))

## Merging the data frames 'test' and 'train' into a single data frame 'merged'

merged<-data.frame(rbind(test,train))
dim(merged)

## Reading the variable names from 'features.txt' file 
## and storing names into a vector 'c.name.mod'

c.name<-read.table("C:/Users/Toshiba/Documents/UCI HAR Dataset/features.txt")
c.name.mod<-as.vector(c.name[,2])

## Incorporating the labels "subject" and "activity

c.name.mod1<-append(c.name.mod,"subject",after=FALSE)
c.name.mod2<-append(c.name.mod1,"activity",after=1)

## Appropriately labelling the data set with descriptive activity names

colnames(merged)<-c.name.mod2

## Extracting the measurements on 'mean()' and 'standard deviation()' for each measurement
# into a modified data frame 'merged.mn.sd'

mn<-grep("mean()",colnames(merged),fixed=TRUE)
sd<-grep("std()",colnames(merged),fixed=TRUE)
mn.sd<-c(mn,sd)
merged.mn.sd<-merged[,c(1,2,mn.sd)]

## clean the names used to name the variables in the data set to make the labels more readable

names(merged.mn.sd)<-gsub("-",".",names(merged.mn.sd))
names(merged.mn.sd)<-gsub("()","",names(merged.mn.sd),fixed=TRUE)

## Exploring the essential features of the data set 'merged.mn.sd'
## e.g.  names, NA values, dimensions.

names(merged.mn.sd)
sum(is.na(merged.mn.sd))
dim(merged.mn.sd)

## Creates a second, independent tidy dataset 'tidydata' 
## (ordered by "subject" and "activity")
## with the average of each variable for each activity and each subject.

install.packages("plyr")
library(plyr)
tidydata<-ddply(merged.mn.sd,.(subject,activity),numcolwise(mean))

## Inserting the activity labels in place of the numerical factor levels in the tidy dataset 
## to improve the readability of the activity levels the data frame 'tidy.data' 
tidydata[,2]<-mapvalues(tidydata[,2],from = c("1","2","3","4","5","6"),
                         to = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

## Exploring features of the tidy dataset named 'tidydata'
## e.g. dimensions, names, the subject and activity columns, head
dim(tidydata)
names(tidydata)
tidydata[,1:2]
head(tidydata)

## Writing 'tidydata' into a text file "tidydata.txt"

write.table(tidydata, file="./tidydata.txt", sep="\t", row.names=FALSE)

## Getting the tidy datset above ("./tidydata.txt") as the output of the script
read.table("./tidydata.txt")



