# Code for extration of image features
# Frankly our model didn't show much difference with the image data
# We extracted length and width for the image as shown in class
# Effect after rescaling also remains same
library(plyr)
library(jpeg)

rm(list=ls())
memory.size(max=T)
#reading the files
leaf_train <- read.csv("C:/Users/navee/Downloads/Leaf_kaggle/train.csv")
leaf_test <- read.csv("C:/Users/navee/Downloads/Leaf_kaggle/test.csv")

numrow=c(rep(0,1584))
numcol=c(rep(0,1584))

leaf_train$ROW_PIXEL=c(rep(0,990))
leaf_train$COL_PIXEL=c(rep(0,990))


leaf_test$ROW_PIXEL=c(rep(0,594))
leaf_test$COL_PIXEL=c(rep(0,594))


#Looking at the variables of leaf_train and leaf_test
#str(leaf_train)
#str(leaf_test)


for(i in 1:1584)
{ fname=paste(i,'.jpg',sep="")
  mypeg=readJPEG(fname,native=F)
  numrow=nrow(mypeg)
  numcol=ncol(mypeg)
  leaf_train<- within(leaf_train, ROW_PIXEL[id == i] <- numrow)
  leaf_train<-within(leaf_train,COL_PIXEL[id==i]<-numcol)
  leaf_test<- within(leaf_test, ROW_PIXEL[id == i] <- numrow)
 leaf_test<-within(leaf_test,COL_PIXEL[id==i]<-numcol)
 }
write.csv(leaf_train,"C:/Users/navee/Downloads/Leaf_kaggle/train.image.csv")
write.csv(leaf_test,"C:/Users/navee/Downloads/Leaf_kaggle/test.image.csv")





library("h2o")
#nthreads -1 means use all cores
#this command also starts a localhost with port 54321 h20 cluster
rm(list = ls())
my_instance <- h2o.init(nthreads = -1)
leaf_train <- h2o.importFile(path = "C:/Users/navee/Downloads/Leaf_kaggle/train.image.csv",header = TRUE)



test <- read.csv("C:/Users/navee/Downloads/Leaf_kaggle/test.image.csv")
leaf_test <- h2o.importFile(path = "C:/Users/navee/Downloads/Leaf_kaggle/test.image.csv",header=TRUE)
# Grid search is the best h20 can provide
# It hels us to tune our parameters better
# sadly the new .grid has a bug
#model_grid
#hidden_layers <- list(c(10,10,10),c(20,20,20),c(25,25))
#activation_func <- c("Tanh", "TanhWithDropout","RectifierWithDropout","Rectifier")
#l1 <- c(1e-5,1e-7)
##hyper_params <- list(hidden_layers,activation_func,l1)
#leaf_grid <- h2o.grid("deeplearning",x=3:196,y=2,training_frame=leaf_train,hyper_params = hyper_params,epoch=75)

# variable_importance should be set true to check rerlative strength of variables under prediction
# having three hidden layers didnt help my model much
# rectifier is the best activation function
# the lossfunction  Cross entropy is used for classification functions
leaf_model <- h2o.deeplearning(x=3:196,y=2,training_frame = leaf_train,standardize = TRUE,hidden = c(35,35),epochs = 150,input_dropout_ratio = 0.2,l1=1e-5,loss = "CrossEntropy",activation = "Rectifier")
leaf_predict <- h2o.predict(object = leaf_model,newdata = leaf_test)

leaf_predict <- as.data.frame(leaf_predict)
leaf_predict <- data.frame(test$id,leaf_predict)
write.csv(leaf_predict,file = "C:/Users/navee/Desktop/test1.csv")
h2o.shutdown()