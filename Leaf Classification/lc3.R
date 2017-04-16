rm(list=ls())
memory.size(max=T)


library(plyr)
library(jpeg)
library(h2o)
library(magick)
#reading the files
leaf_train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\leaf_train.csv")
leaf_test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\leaf_test.csv")



numrow=c(rep(0,1584))
numcol=c(rep(0,1584))

leaf_train$ROW_PIXEL=c(rep(0,990))
leaf_train$COL_PIXEL=c(rep(0,990))

leaf_train$s=c(rep(0,990))




leaf_test$ROW_PIXEL=c(rep(0,594))
leaf_test$COL_PIXEL=c(rep(0,594))


leaf_test$s=c(rep(0,594))

#Looking at the variables of leaf_train and leaf_test
#str(leaf_train)
#str(leaf_test)


#Finding the columns that have NA values
sapply(leaf_train, function(x)all(is.na(x)))
sapply(leaf_test, function(x)all(is.na(x)))


for(i in 1:1584)
{
  fname=paste(i,'.jpg',sep="")
  mypeg <- image_read(fname)
  size<-image_info(mypeg)$filesize
  numrow=image_info(mypeg)$width
  numcol=image_info(mypeg)$height
  leaf_train<- within(leaf_train, ROW_PIXEL[id == i] <- numrow)
  leaf_train<-within(leaf_train,COL_PIXEL[id==i]<-numcol)
  leaf_train<-within(leaf_train,s[id==i]<-size)
  leaf_test<- within(leaf_test, ROW_PIXEL[id == i] <- numrow)
  leaf_test<-within(leaf_test,COL_PIXEL[id==i]<-numcol)
  leaf_test<-within(leaf_test,s[id==i]<-size)
}


#There are no missing NA values in either of the data set
leaf_train1<-leaf_train[,3:197]
leaf_test1<-leaf_test[,3:196]
#cor(leaf_train1)


leaf_train1.margin <- leaf_train1[,1:64]
leaf_test1.margin<-leaf_test1[,1:64]

leaf_train1.shape<-leaf_train1[,65:128]
leaf_test1.shape<-leaf_test1[,65:128]

leaf_train1.texture<-leaf_train1[,129:192]
leaf_test1.texture<-leaf_test1[,129:192]

#leaf_train1.width<-leaf_train1[,193]
#leaf_test1.width<-leaf_train1[,193]


#leaf_train1.height<-leaf_train1[,194]
#leaf_test1.height<-leaf_test1[,194]

#leaf_train1.depth<-leaf_train1[,195]
#leaf_test1.depth<-leaf_test1[,195]




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
train.data <- data.frame(Species= leaf_train$species,leaf_pca_margin$x,leaf_pca_shape$x,leaf_pca_texture$x,row=leaf_train$ROW_PIXEL,col=leaf_train$COL_PIXEL,size=leaf_train$s)
test.data<-data.frame(leaf_pca_margin1$x,leaf_pca_shape1$x,leaf_pca_texture1$x,row=leaf_test$ROW_PIXEL,col=leaf_test$COL_PIXEL,size=leaf_test$s)

#names(test.data)
#names(train.data)

#we are interested in first 30 PCAs
train.data <- train.data[, c(1:30,65:95,129:159,194:196)]
test.data<-test.data[, c(1:30,65:95,129:159,194:196)] 

gautham_instance <- h2o.init(nthreads = -1)

train.data <- as.h2o(train.data)
test.data1 <- as.h2o(test.data)

#run a decision tree
leaf_model <- h2o.deeplearning(x=2:9,y=1,training_frame = train.data,hidden = 5,epochs = 25,loss ="CrossEntropy" )



#transform test into PCA
#test.data <- predict(leaf_pca, newdata = leaf_test)
#test.data <- as.data.frame(test.data)

#select the first 30 components
#test.data <- test.data[,1:30]

#head(test.data)

#make prediction on test data
leaf_predict <- h2o.predict(object = leaf_model,newdata = test.data1)


leaf_predict <- as.data.frame(leaf_predict)
leaf_predict <- data.frame(leaf_test$id,leaf_predict)
write.csv(leaf_predict,"C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Leaf Classification\\file_sub3.csv")

