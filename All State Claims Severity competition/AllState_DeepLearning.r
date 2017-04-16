#library(xgboost)

#train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\train.csv")
#test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\test.csv")


#Sum(is.na(train))
#sum(is.na(test))


#The simplest training command is as follows:
  
  #model <- xgboost(data = train, label = train$loss, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

#preds = predict(model, test$data)

#Sub_1<- data.frame(preds)
#write.csv(Sub_1,'C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity Competitionr\\file_sub_XG.csv',row.names=FALSE)


#===============================================================================================================================================================================
  

#library(nmf)

#train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\train.csv")
#test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\test.csv")

#train_a <- as.matrix(train[,118:132])

#res<-nmf(train_a, 15,"lee")

#V.hat <- fitted(res) 
#print(V.hat)



#=================================================================================================================================================================================

library(FactoMineR)

#Maximising the memory and clearing the environment
rm(list=ls())
memory.size(max=T)


train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\train.csv")
test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\test.csv")

train_res<-MCA(train[,c(2:115)])


#==================================================================================================================================================================================


#Maximising the memory and clearing the environment
#rm(list=ls())
#memory.size(max=T)

#library(MASS)



#train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\train.csv")
#test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\test.csv")

#newtrain = train[, c(2:116)]

#train_res<-mca(newtrain)

#==================================================================================================================================================================================

#Maximising the memory and clearing the environment
#rm(list=ls())
#memory.size(max=T)

#library(homals)



#train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\train.csv")
#test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity competition\\test.csv")

#newtrain = train[, c(2:116)]

#train_res= homals(newtrain, ndim = 115, level = "nominal")

#==================================================================================================================================================================================


