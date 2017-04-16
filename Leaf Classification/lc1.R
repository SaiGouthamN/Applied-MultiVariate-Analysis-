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


leaf_train1.margin <- leaf_train1[,1:64]
leaf_test1.margin<-leaf_test1[,1:64]

leaf_train1.shape<-leaf_train1[,64:128]
leaf_test1.shape<-leaf_test1[,64:128]

leaf_train1.texture<-leaf_train1[,128:190]
leaf_test1.texture<-leaf_test1[,128:190]


#Performing principal component analysis
leaf_pca_margin<-prcomp(leaf_train1.margin)
leaf_pca_shape<-prcomp(leaf_train1.shape)
leaf_pca_texture<-prcomp(leaf_train1.texture)
#summary(leaf_pca)
#names(leaf_pca)


#Performing principal component analysis
leaf_pca_margin1<-prcomp(leaf_test1.margin)
leaf_pca_shape1<-prcomp(leaf_test1.shape)
leaf_pca_texture1<-prcomp(leaf_test1.texture)
#summary(leaf_pca1)
#names(leaf_pca1)



#add a training set with principal components
train.data <- data.frame(Species= leaf_train$species,leaf_pca_margin$x,leaf_pca_shape$x,leaf_pca_texture$x)
test.data<-data.frame(Species=leaf_test$id,leaf_pca_margin1$x,leaf_pca_shape1$x,leaf_pca_texture1$x)

#we are interested in first 30 PCAs
train.data <- train.data[, c(1,65:67,129:130)]
test.data<-test.data[, c(1,65:67,129:130)]



#run a decision tree
rpart.model <- multinom(Species ~ .,data = train.data,method='class')

#transform test into PCA
#test.data <- predict(leaf_pca, newdata = leaf_test)
#test.data <- as.data.frame(test.data)

#select the first 30 components
#test.data <- test.data[,1:30]

#head(test.data)

#make prediction on test data
rpart.prediction <- predict(rpart.model,test.data,type='prob')

#Submit the values to the output file
Sub_1<- data.frame(id=leaf_test$id,rpart.prediction)
write.csv(Sub_1,'C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\file_sub1.csv',row.names=FALSE)
