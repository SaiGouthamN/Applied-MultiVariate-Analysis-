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

leaf_train_margin<-leaf_train[,3:66]
leaf_train_texture<-leaf_train[,67:130]
leaf_train_shape<-leaf_train[,131:194]

for( i in seq(1,63,2))
{
  leaf_train_margin[i]<-(leaf_train_margin[i] + leaf_train_margin[i+1])/2
}
leaf_train_margin<-leaf_train_margin[,1:32]

for( i in seq(1,63,2))
{
  leaf_train_texture[i]<-(leaf_train_texture[i] + leaf_train_texture[i+1])/2
}
leaf_train_texture<-leaf_train_texture[,1:32]

for( i in seq(1,63,2))
{
  leaf_train_shape[i]<-(leaf_train_shape[i] + leaf_train_shape[i+1])/2
}
leaf_train_shape<-leaf_train_shape[,1:32]


leaf_test_margin<-leaf_test[,2:65]
leaf_test_texture<-leaf_test[,66:129]
leaf_test_shape<-leaf_test[,130:193]

for( i in seq(1,63,2))
{
  leaf_test_margin[i]<-(leaf_test_margin[i] + leaf_test_margin[i+1])/2
}

leaf_test_margin<-leaf_test_margin[,1:32]

for( i in seq(1,63,2))
{
  leaf_test_texture[i]<-(leaf_test_texture[i] + leaf_test_texture[i+1])/2
}
leaf_test_texture<-leaf_test_texture[,1:32]
for( i in seq(1,63,2))
{
  leaf_test_shape[i]<-(leaf_test_shape[i] + leaf_test_shape[i+1])/2
}
leaf_test_shape<-leaf_test_shape[,1:32]



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







#add a training set with principal components
train.data <- data.frame(Species= leaf_train$species,leaf_train_margin,leaf_train_shape,leaf_train_texture,row=leaf_train$ROW_PIXEL,col=leaf_train$COL_PIXEL,size=leaf_train$s)
test.data<-data.frame(leaf_test_margin,leaf_test_shape,leaf_test_texture,row=leaf_test$ROW_PIXEL,col=leaf_test$COL_PIXEL,size=leaf_test$s)


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

