library(plyr)
library(rpart)
#reading the files
leaf_train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\leaf_train.csv")
leaf_test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\leaf_test.csv")


#Looking at the variables of leaf_train and leaf_test
#str(leaf_train)
#str(leaf_test)


#Finding the columns that have NA values
sapply(leaf_train, function(x)all(is.na(x)))
sapply(leaf_test, function(x)all(is.na(x)))

#There are no missing NA values in either of the data set
leaf_train1<-leaf_train[,3:193]
leaf_test1<-leaf_test[,3:193]
#cor(leaf_train1)

#Performing principal component analysis
leaf_pca<-prcomp(leaf_train1,scores=TRUE,cor=TRUE)
#summary(leaf_pca)
#names(leaf_pca)


#Performing principal component analysis
leaf_pca1<-prcomp(leaf_test1,scores=TRUE,cor=TRUE)
#summary(leaf_pca1)
#names(leaf_pca1)



#add a training set with principal components
train.data <- data.frame(Species= leaf_train$species, leaf_pca$x)
test.data<-data.frame(Species=leaf_test$id,leaf_pca1$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:31]
test.data<-test.data[,1:31]

#run a decision tree
 rpart.model <- rpart(Species ~ .,data = train.data, method = "class")
 rpart.model

#transform test into PCA
#test.data <- predict(leaf_pca, newdata = leaf_test)
#test.data <- as.data.frame(test.data)

#select the first 30 components
#test.data <- test.data[,1:30]
 
#head(test.data)

#make prediction on test data
 rpart.prediction <- predict(rpart.model,test.data, method="class")

#Submit the values to the output file
Sub_1<- data.frame(id=leaf_test$id,rpart.prediction)
write.csv(Sub_1,'C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\file_sub.csv',row.names=FALSE)
