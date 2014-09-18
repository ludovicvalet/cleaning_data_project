

################################################################################################
###################### merge subjects one column
################################################################################################


### read subjects file test and train

subjects_test <- read.csv("test/subject_test.txt", sep=" ", stringsAsFactors=FALSE)

subjects_train <- read.csv("train/subject_train.txt", sep=" ", stringsAsFactors=FALSE)

##name column subjects_test and subjects_train
names(subjects_test)[1]<-"subjects"
names(subjects_train)[1]<-"subjects"

## merge subjects_test and subjects_train

subjects_all<-rbind(subjects_test,subjects_train)



################################################################################################
###################### merge activities one column
################################################################################################


### read activities file test and train

activities_test <- read.csv("test/y_test.txt", sep="", stringsAsFactors=FALSE)

activities_train <- read.csv("train/y_train.txt", sep="", stringsAsFactors=FALSE)

activities_labels <- read.csv("activity_labels.txt", sep="", stringsAsFactors=FALSE,header = FALSE,col.names=c("number","activity"))




##name column subjects_test and subjects_train
names(activities_test)[1]<-"activities"
names(activities_train)[1]<-"activities"

## merge subjects_test and subjects_train

activities_all<-rbind(activities_test,activities_train)


###rename activities

v<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING"
    ,"STANDING","LAYING")
temp<-activities_all
for(i in c(1:10297))
{
    activities_all[i,1]<-v[temp[i,1]]
}

################################################################################################
###################### merge features
################################################################################################


X_test <- read.csv("test/X_test.txt", sep="", stringsAsFactors=FALSE)

X_train <- read.csv("train/X_train.txt", sep="", stringsAsFactors=FALSE)

X_labels<- read.csv("features.txt", sep="", stringsAsFactors=FALSE, header = FALSE,col.names=c(1,"feature"))

##name column activities



var_names<-as.vector(X_labels[,2])

names(X_test)<-var_names
names(X_train)<-var_names


## merge features X_test and X_train
X_all<-rbind(X_test,X_train)


#### selection feature mean and sd

names_col<-names(X_all)

select<-grep("mean|std",names_col)

select_col<-X_all[select]



################################################################################################
###################### merge all data
################################################################################################

data_all<-cbind(subjects_all,activities_all,select_col)



################################################################################################
###################### proceed with datas step 5
### ###################
#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.
### ###################
################################################################################################


v<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING"
     ,"STANDING","LAYING")
temp_all<-data.frame(c())
count<-0
names(temp_all)<-names(data_all)
for(i in c(1:30))##select subject
    { ##count line of final data frame
    for(j in c(1:6))##select activity
        {count<-count+1
         ##extract per subject
        temp_temp1<-data_all[data_all$subjects==i,]
        ##extract per activity
        temp_temp2<-temp_temp1[temp_temp1$activities==v[j],]
        ###calcul mean
        temp_temp3<-data.frame(c())
        moyenne<-colMeans(temp_temp2[,c(3:81)])
        temp_temp3[1,1]<-i     ##subject i
        temp_temp3[1,2]<-v[j]  ##activity j
        for(k in c(1:79)){temp_temp3[1,k+2]<-moyenne[k]}
        ##add a line in final data frame
        for(l in c(1:81)){temp_all[count,l]<-temp_temp3[1,l]}
        }
    }

write.table(temp_all,"mean_per_sub_act.txt")

new<-read.csv("mean_per_sub_act.txt",sep=" ")