Getting and cleaning Data : Assignment
======================================

"Human Activity Recognition Using Smartphones Dataset Version 1.0"
-----------------------------------------------------------------

```

"Experiments on 30 subjects in the age group of 19-48 were carried out. 
Each person performed six activities wearing a smartphone (Samsung Galaxy S II) on the waist. 
These activities are :
1.WALKING, 
2.WALKING_UPSTAIRS, 
3.WALKING_DOWNSTAIRS, 
4.SITTING, 
5.STANDING, 
6.LAYING

 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz were captured using the smartphone's embedded accelerometer and gyroscope. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 
50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth 
low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz 
cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 
 The obtained dataset was randomly partitioned into two sets, where 70% of the volunteers were selected for generating the training data and 30% the test data. 
 **For the purpose of this analysis, we merged the test and train datasets into a single dataset 'merged', then extracting (subsetting) those variables that averaged out the mean and standard deviation measurements. The variables were then cleaned into a descriptive more readable form.**

For each record the following information has been provided:

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body    acceleration.
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

For the purpose of this analysis, following files from the 'UCI HAR dataset' have been referred:
* 'README.txt'
* 'features_info.txt': Shows information about the variables used on the feature vector.
* 'features.txt': List of all features.
* 'activity_labels.txt': Links the class labels with their activity name.
* 'train/X_train.txt': Training set.
* 'train/y_train.txt': Training labels.
* 'test/X_test.txt': Test set.
* 'test/y_test.txt': Test labels.


### Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file." **[1]**
- The UCI HAR dataset was downloaded for the purpose into the working directory.**[2]**

### Files included for the purpose of this project :

- 'READme.md'     : Brief Introduction about the project
- 'run_Analysis.R': The code script in R.
- 'CodeBook.md'   : details the data viz names, dimensions, tidying, transformations;
                    and explaining the code 

### REFERENCEs :
1. UCI HAR dataset : 'README.txt'
2. Coursera link to the UCI HAR dataset :
   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

```



