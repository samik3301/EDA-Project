#EDA Jcomp

#Loading necessary libraries/installing packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("e1071")
#library(e1071)
#install.packages("caTools")
library(caTools)
#install.packages("caret")
library(caret)
#install.packages("skimr")
library(skimr)
#install.packages("janitor")
library(janitor)
#install.packages("class")
library(class)
#loading the raw extracted datasets
activity_data_raw <-read.csv("ACTIVITY_1566153601293.csv")
heartrate_data_raw <-read.csv("HEARTRATE_AUTO_1566153602880.csv")
sleep_data_raw <-read.csv("SLEEP_1566153602529.csv")

#checking the overview 
str(activity_data_raw)
str(heartrate_data_raw)
str(sleep_data_raw)

#printing the sumary of the datasets
skim_without_charts(activity_data_raw)
skim_without_charts(heartrate_data_raw)
skim_without_charts(sleep_data_raw)

#checking for any 'Not Availabe' or missing values 
anyNA(activity_data_raw, recursive = FALSE)
anyNA(heartrate_data_raw, recursive = FALSE)
anyNA(sleep_data_raw, recursive = FALSE)

#cleaning data by removing entries containing errorenous entries
#example [0 in steps, calories, runDistance etc]
activity_data_new<-activity_data_raw[activity_data_raw$steps!=0 &activity_data_raw$distance!=0 &activity_data_raw$distance!=0,]
head(activity_data_new)
skim_without_charts(activity_data_new)

#PreProcessing the dataset
activity_data_processed <- activity_data_new %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"), 
         distance = distance/1000, very_active_distance = run_distance/1000)%>%  
  select(c(date,distance, very_active_distance, steps, calories,)) %>% 
  arrange(date)

#Cleaned the names of the variables
#removed repeated data, only considered unique data [distinct]
#changed the date from the dataset to dateType format
#converted the distance from metres to kilometres  and run_distance to kilometres as well for better readablility
#sorted by the dates


#now introducing the target class variable based on the number of steps in activity 
#if number of steps is <5000 then they fall under the risk of developing heart disease badly 
#else they dont fall in the risk [assumption]
# https://med.stanford.edu/news/all-news/2017/05/fitness-trackers-accurately-measure-heart-rate-but-not-calories-burned.html
#why we didnt consider the calories burned data into the HeartDisease Prediction Label Class

#1 denotes the risk
HeartDisease <-c()

for (x in 1:nrow(activity_data_processed)){
  if(activity_data_processed$steps[x] < 5000){
    HeartDisease <-append(HeartDisease,1)
  }
  else{
    HeartDisease <- append(HeartDisease,0)
  }
}

table(HeartDisease)

#added the label to the dataframe
activity_data_processed['HeartDiseaseLabel_Activity'] <-HeartDisease

head(activity_data_processed)
skim_without_charts(activity_data_processed) #number of rows reduced after cleaning the duplicates

#removing errorenous heartRate if present 
heartrate_data_new <-heartrate_data_raw[heartrate_data_raw$heartRate!=0,]

skim_without_charts(heartrate_data_new) #number of rows didnt change, no errorenous data[heart rate = 0] is present

#PreProcessing the dataset
heartrate_data_processed <- heartrate_data_new %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         time = substr(time,1,5)) %>% 
  select(c(date, time, heart_rate)) %>% 
  arrange(date,time)

#Cleaned the names
#removed repeated entries (kept the unique ones)
#Converted the date from the Dataset into Date Type formatting
#Removed the seconds info from the 'time' column
#Sorted based on the date and time

head(heartrate_data_processed)
skim_without_charts(heartrate_data_processed)

#here if the heart rate is more than 100 and doesnt change for 8 minutes since 1 entry is 2 mins interval frequency for recording the heart rate
#there is a risk of heart disease 
#1 denotes risk
HeartDisease_heart <-c()
for (x in 1:nrow(heartrate_data_processed)){
  if(heartrate_data_processed$heart_rate[x] > 100 &heartrate_data_processed$heart_rate[x+1] > 100 & heartrate_data_processed$heart_rate[x+2] > 100 & heartrate_data_processed$heart_rate[x+3] > 100 &heartrate_data_processed$heart_rate[x+4] > 100){
    HeartDisease_heart <-append(HeartDisease_heart,1)
  }
  else{
    HeartDisease_heart <- append(HeartDisease_heart,0)
  }
}

table(HeartDisease_heart)

heartrate_data_processed['HeartDisease_Label_HR'] <-HeartDisease_heart
head(heartrate_data_processed)

skim_without_charts(sleep_data_raw)
#removing the rows where deepSleep, shallowSleep is both 0 [sleep data didnt get recorded or errorenous data(corrupted)]
sleep_data_new <- sleep_data_raw[sleep_data_raw$deepSleepTime!=0 & sleep_data_raw$shallowSleepTime!=0,]
head(sleep_data_new)
skim_without_charts(sleep_data_new) #number of rows reduced 

#removed the rows where the data is corrupted [deepSleepTime = 0 and shallowSleepTime= 0]

#PreProcessing the dataset
sleep_data_processed <- sleep_data_new %>% 
  clean_names() %>% 
  distinct() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         sleep_time = deep_sleep_time + shallow_sleep_time,
         bed_time = deep_sleep_time + shallow_sleep_time + wake_time) %>% 
  select(c(date, deep_sleep_time, shallow_sleep_time,
           sleep_time, bed_time, wake_time)) %>%
  arrange(date)

#bed_time includes the time being awake and in bed as well as time slept

#cleaned the names of columns
#removed the repeated entries and considered only the unique/distinct
#changed the date from dataset into Date format
#removed sync_time, start, stop columns
#created sleep_time, bed_time
#sorted based on the date

head(sleep_data_processed)
skim_without_charts(sleep_data_processed)

#if the sleep_time is less than 360mins [6hrs] then possible risk of heart disease
#source: https://www.sleepfoundation.org/sleep-faqs/is-6-hours-of-sleep-enough
#1 denotes risk
HeartDisease_sleep <-c()
for (x in 1:nrow(sleep_data_processed)){
  if(sleep_data_processed$sleep_time[x] < 360){
    HeartDisease_sleep <-append(HeartDisease_sleep,1)
  }
  else{
    HeartDisease_sleep <- append(HeartDisease_sleep,0)
  }
}

#adding the Class label to the dataframe
sleep_data_processed['HeartDisease_Label_timeSlept'] <-HeartDisease_sleep

table(HeartDisease_sleep)

head(sleep_data_processed)
skim_without_charts(sleep_data_processed)

#Now all three datasets have been processed, applying various algorithms
#Linear SVM

head(activity_data_processed)
#considering distance, very_active_distance+steps,calories and Class Label for the algorithm
activity_data_processed_considered <- activity_data_processed[2:6]

split_linear_svm = sample.split(activity_data_processed_considered$HeartDiseaseLabel_Activity, SplitRatio = 0.75)
training_set_linear_svm = subset(activity_data_processed_considered, split_linear_svm == TRUE)
test_set_linear_svm = subset(activity_data_processed_considered, split_linear_svm == FALSE)

# Feature scaling of the attributes
training_set_linear_svm[-5] = scale(training_set_linear_svm[-5])
test_set_linear_svm[-5] = scale(test_set_linear_svm[-5])

#now creating the model
classifier_svm = svm(formula = HeartDiseaseLabel_Activity ~ .,
                 data = training_set_linear_svm,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the test set
y_pred_svm_linear = predict(classifier_svm, newdata = test_set_linear_svm[-5])

# Evaluating the model
cm_svm = table(test_set_linear_svm[, 5], y_pred_svm_linear)
diag_svm = diag(cm_svm)
rowsums_svm = apply(cm_svm, 1, sum)
colsums_svm = apply(cm_svm, 2, sum)
accuracy_svm = sum(diag_svm) / sum(cm_svm)
precision_svm = diag_svm / colsums_svm
recall_svm = diag_svm / rowsums_svm
f1_svm = 2 * precision_svm * recall_svm / (precision_svm + recall_svm)
data.frame(accuracy_svm, precision_svm, recall_svm, f1_svm) 

#Visualization Part 
#left to do because ElemStatLearn got removed from CRAN ded
#for training data

#for testing data


head(heartrate_data_processed)
#considering heart_rate and Class Label for the algorithm
heartrate_data_processed_considered <- heartrate_data_processed[3:4]

split_linear_svm_heart = sample.split(heartrate_data_processed_considered$HeartDisease_Label_HR, SplitRatio = 0.75)
training_set_linear_svm_heart = subset(heartrate_data_processed_considered, split_linear_svm_heart == TRUE)
test_set_linear_svm_heart = subset(heartrate_data_processed_considered, split_linear_svm_heart == FALSE)

# Feature scaling of the attributes
training_set_linear_svm_heart[-2] = scale(training_set_linear_svm_heart[-2])
test_set_linear_svm_heart[-2] = scale(test_set_linear_svm_heart[-2])

#now creating the model
classifier_svm_heart = svm(formula = HeartDisease_Label_HR ~ .,
                     data = training_set_linear_svm_heart,
                     type = 'C-classification',
                     kernel = 'linear')

# Predicting the test set
y_pred_svm_linear_heart = predict(classifier_svm_heart, newdata = test_set_linear_svm_heart[-2])

# Evaluating the model
cm_svm_heart = table(test_set_linear_svm_heart[, 2], y_pred_svm_linear_heart)
diag_svm_heart = diag(cm_svm_heart)
rowsums_svm_heart = apply(cm_svm_heart, 1, sum)
colsums_svm_heart = apply(cm_svm_heart, 2, sum)
accuracy_svm_heart = sum(diag_svm_heart) / sum(cm_svm_heart)
precision_svm_heart = diag_svm_heart / colsums_svm_heart
recall_svm_heart = diag_svm_heart / rowsums_svm_heart
f1_svm_heart = 2 * precision_svm_heart * recall_svm_heart / (precision_svm_heart + recall_svm_heart)
data.frame(accuracy_svm_heart, precision_svm_heart, recall_svm_heart, f1_svm_heart) 

#Visualization part
#for training data

#for testing data


head(sleep_data_processed)
#considering sleep_time,bed_time,wake_time and Class Label for the algorithm
sleep_data_processed_considered <- sleep_data_processed[4:7]

split_linear_svm_sleep = sample.split(sleep_data_processed_considered$HeartDisease_Label_timeSlept, SplitRatio = 0.75)
training_set_linear_svm_sleep = subset(sleep_data_processed_considered, split_linear_svm_sleep == TRUE)
test_set_linear_svm_sleep = subset(sleep_data_processed_considered, split_linear_svm_sleep == FALSE)

# Feature scaling of the attributes
training_set_linear_svm_sleep[-4] = scale(training_set_linear_svm_sleep[-4])
test_set_linear_svm_sleep[-4] = scale(test_set_linear_svm_sleep[-4])

#now creating the model
classifier_svm_sleep = svm(formula = HeartDisease_Label_timeSlept ~ .,
                           data = training_set_linear_svm_sleep,
                           type = 'C-classification',
                           kernel = 'linear')

# Predicting the test set
y_pred_svm_linear_sleep = predict(classifier_svm_sleep, newdata = test_set_linear_svm_sleep[-4])

# Evaluating the model
cm_svm_sleep = table(test_set_linear_svm_sleep[, 4], y_pred_svm_linear_sleep)
diag_svm_sleep = diag(cm_svm_sleep)
rowsums_svm_sleep = apply(cm_svm_sleep, 1, sum)
colsums_svm_sleep = apply(cm_svm_sleep, 2, sum)
accuracy_svm_sleep = sum(diag_svm_sleep) / sum(cm_svm_sleep)
precision_svm_sleep = diag_svm_sleep / colsums_svm_sleep
recall_svm_sleep = diag_svm_sleep / rowsums_svm_sleep
f1_svm_sleep = 2 * precision_svm_sleep * recall_svm_sleep / (precision_svm_sleep + recall_svm_sleep)
data.frame(accuracy_svm_sleep, precision_svm_sleep, recall_svm_sleep, f1_svm_sleep) 

#Visualization part
#for training data

#for testing data

#KNN 
head(activity_data_processed)

activity_data_processed$HeartDiseaseLabel_Activity = factor(activity_data_processed$HeartDiseaseLabel_Activity, level = c(0, 1))
activity_data_processed_considered <- activity_data_processed[,2:5]

#feature scaling 
training_set_knn_activity[,2:4] <-scale(training_set_knn_activity[,2:4])
testing_set_knn_activity[,2:4]<-scale(testing_set_knn_activity[,2:4])

k_approx = floor(sqrt(nrow(activity_data_processed_considered))) #can take elbow method also but eh

#now creating the model
y_pred_knn_activity = knn(
  train = training_set_knn_activity[,2:4],
  test = testing_set_knn_activity[,2:4],
  cl = training_set_knn_activity[,5],
  k= k_approx
)

cm = table(testing_set_knn_activity[,5],y_pred_knn_activity)
cm


#visualization of KNN using plot


#KNN on the heart dataset
heartrate_data_processed_considered <- heartrate_data_processed[3:4]
heartrate_data_processed_considered$HeartDisease_Label_HR <-factor(heartrate_data_processed_considered$HeartDisease_Label_HR,level = c(0,1))


head(heartrate_data_processed_considered)

split_knn_heart <-sample.split(heartrate_data_processed_considered$HeartDisease_Label_HR,SplitRatio = 0.75)
training_set_knn_heart <-subset(heartrate_data_processed_considered,split_knn_heart == TRUE)
testing_set_knn_heart <-subset(heartrate_data_processed_considered,split_knn_heart == FALSE)

#feature scaling 
training_set_knn_heart[,1] <-scale(training_set_knn_heart[,1])
testing_set_knn_heart[,1]<-scale(testing_set_knn_heart[,1])

k_approx_heart = floor(sqrt(nrow(heartrate_data_processed_considered))) #can take elbow method also but eh

#now creating the model
y_pred_knn_heart = knn(
  train = training_set_knn_heart[,1],
  test = testing_set_knn_heart[,1],
  cl = training_set_knn_heart[,2],
  k= 1
)

cm_heart = table(testing_set_knn_heart[,2],y_pred_knn_heart)

#data visualization on results

#KNN on the sleep dataset

head(sleep_data_processed_considered)
sleep_data_processed_considered$HeartDisease_Label_timeSlept <-factor(sleep_data_processed_considered$HeartDisease_Label_timeSlept,levels = c(0,1))
split_knn_sleep <-sample.split(sleep_data_processed_considered$HeartDisease_Label_timeSlept,SplitRatio = 0.75)
training_set_knn_sleep <-subset(sleep_data_processed_considered,split_knn_sleep == TRUE)
testing_set_knn_sleep <-subset(sleep_data_processed_considered,split_knn_sleep == FALSE)

#feature scaling 
training_set_knn_sleep[,1:3] <-scale(training_set_knn_sleep[,1:3])
testing_set_knn_sleep[,1:3]<-scale(testing_set_knn_sleep[,1:3])

k_approx_sleep = floor(sqrt(nrow(sleep_data_processed_considered))) #can take elbow method also but eh

#now creating the model
y_pred_knn_sleep = knn(
  train = training_set_knn_sleep[,1:3],
  test = testing_set_knn_sleep[,1:3],
  cl = training_set_knn_sleep[,4],
  k= k_approx_sleep
)

cm_sleep = table(testing_set_knn_sleep[,4],y_pred_knn_sleep)
cm_sleep

#visualization analysis of the sleep KNN

#before adding data visualization and 2 more algorithms


