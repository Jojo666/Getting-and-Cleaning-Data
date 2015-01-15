features     = read.table("features.txt",header=FALSE); #imports features.txt
activityType = read.table("activity_labels.txt",header=FALSE); #imports activity_labels.txt
subjectTrain = read.table("subject_train.txt",header=FALSE); #imports subject_train.txt
xTrain       = read.table("x_train.txt",header=FALSE); #imports x_train.txt
yTrain       = read.table("y_train.txt",header=FALSE); #imports y_train.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# 1)training data set

trainingData = cbind(yTrain,subjectTrain,xTrain);

##test data set

subjectTest = read.table("subject_test.txt",header=FALSE); #imports subject_test.txt
xTest       = read.table("x_test.txt",header=FALSE); #imports x_test.txt
yTest       = read.table("y_test.txt",header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# 2)Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);

## 3) Merge test and train data

finalData = rbind(trainingData,testData);

colNames  = colnames(finalData); 

##Extract values for mean and SD

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

## subset when values of logical vector are true

finalData = finalData[logicalVector==TRUE];

finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

colNames  = colnames(finalData); 

# 4) Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};


colnames(finalData) = colNames;

finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# 5) Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

