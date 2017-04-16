#Loading important libraries
library(caret)
library(stats)
library(e1071)

#Maximising the memory
rm(list=ls())
memory.size(max=T)


#Accepting the testing and training data
train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Digit Recognizer\\train.csv")
test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Digit Recognizer\\test.csv")


#Finding the number of NA values in training and the test set
sum(is.na(train))
sum(is.na(test))


#Converting the label column in training set to factors
train$label<-as.factor(train$label)

#Scaling the training and the testing data set
train_scale<-scale(train[,c(2:785)])
test_scale<-scale(test)


#Finding the near zero variance columns in training set
nzv<-nearZeroVar(train_scale)

if (length(nearZeroVar(train_scale)) > 0) {
  train_scale <- train_scale[, -nzv] 
}


#Finding the near zero variance columns in test set
nzv<-nearZeroVar(test_scale)

if (length(nearZeroVar(test_scale)) > 0) {
  test_scale <- test_scale[, -nzv] 
}




#Performing Principal Component Analysis(PCA) on training and test data
train_pca<-prcomp(train_scale,scale.=TRUE)
test_pca<-prcomp(test_scale,scale.=TRUE)

#add a training set with principal components
train.data <- data.frame(label=train$label,train_pca$x)
test.data<-data.frame(test_pca$x)

#we are interested in first 100 PCAs
train.data <- train.data[, c(1:41)]
test.data<-test.data[, c(1:41)] 


#run a decision tree
tuneResult <- tune(svm, label ~ .,  data = train.data,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)



#make prediction on test data
rpart.prediction <- predict(svm_model,test.data)

#Submit the values to the output file
Sub_1<- data.frame(ImageId=seq(1,28000),label=rpart.prediction)
write.csv(Sub_1,'C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Digit Recognizer\\file_sub_SVM.csv',row.names=FALSE)



