#Maximising the memory and clearing the environment
rm(list=ls())
memory.size(max=T)

#Loading important libraries
library(caret)
library(stats)
library(randomForest)

#Accepting the testing and training data
train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Digit Recognizer\\train.csv")
test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Digit Recognizer\\test.csv")

#Finding the number of NA values in training and the test set.
#And it can be noted that there are not missing values in both the dataset.
sum(is.na(train))
sum(is.na(test))

#Converting the label column in training set to factors.
#It is important to convert the dependent variaable label to factors to avoid getting fractional values in the predicted out instead of whole numbers
train$label<-as.factor(train$label)

#It is highly advisable to scale the dataset before performing principal component analysis.
#In the train set we are scaling everything except the 'label' field as it a categorical(character/non-numeric) variable
#Scaling the training and the testing data set
train_scale<-scale(train[,c(2:785)])
test_scale<-scale(test)

#To Perform PCA it is important to have variables that don not have zero/null varaiance.
#There is a function in the caret package which lets you indentify the uniqueness of a field in a data set.
#We are removing the columns that have near zero variance
#Finding the near zero variance columns in training set using nearZeroVar().

#Finding the near zero variance columns in train set and removing them
nzv<-nearZeroVar(train_scale)
if (length(nearZeroVar(train_scale)) > 0) {
  train_scale <- train_scale[, -nzv] 
}

#Number of columns in the test set after removal of near zero variance columns
ncol(train_scale)

#Finding the near zero variance columns in test set and removing them
nzv<-nearZeroVar(test_scale)
if (length(nearZeroVar(test_scale)) > 0) {
  test_scale <- test_scale[, -nzv] 
}

#Number of columns in the test set after removal of near zero variance columns
ncol(test_scale)

#Performing Principal Component Analysis(PCA) on training and test data
#Since there are 785 independent variables, dimension reduction is highly important to reduce the dimensions.
#prcomp is a function in R which lets you perform dimension reduciton
train_pca<-prcomp(train_scale,scale.=TRUE)
test_pca<-prcomp(test_scale,scale.=TRUE)

#Create new data frames train.data and test.data with their respective PCA values.
#Add a column label to the training set as that is the independent variable to be predicted.
train.data <- data.frame(label=train$label,train_pca$x)
test.data<-data.frame(test_pca$x)

#we are interested in first 41 PCAs as it explains 81% of the varaince of the whole dataset.
train.data <- train.data[, c(1:41)]
test.data<-test.data[, c(1:41)] 


#run a random forest model with mtry as 9 and ntree as 11000 as they are the most optimal hyper parameters as per the RFTune function.
# Algorithm Tune (tuneRF)
#set.seed(7)
#bestmtry <- tuneRF(train.data, train.data$label, stepFactor=1.5, improve=1e-5, ntree=1100)
#print(bestmtry) which resulted that mtry =9  gives the most accurate result.Therefore i chose mtry=9
rf.model <- randomForest(label ~ .,data = train.data,method='class',mtry=9,ntree=1000)

#Summarising the model
summary(rf.model)

#make prediction on test data
rf.prediction <- predict(rf.model,test.data)

#Submit the values to the output file
Sub_1<- data.frame(ImageId=seq(1,28000),label=rf.prediction)
write.csv(Sub_1,'C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Digit Recognizer\\file_sub_RF.csv',row.names=FALSE)

 