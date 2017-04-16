#Maximising the memory and clearing the environment
rm(list=ls())
memory.size(max=T)

#Loading important libraries
library(caret)
library(stats)
library(h2o)

#Accepting the testing and training data
train<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity Competition\\train.csv")
test<-read.csv("C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity Competitionr\\test.csv")

#Finding the number of NA values in training and the test set.
#And it can be noted that there are not missing values in both the dataset.
sum(is.na(train))
sum(is.na(test))

#Converting the label column in training set to factors.
#It is important to convert the dependent variaable label to factors to avoid getting fractional values in the predicted out instead of whole numbers
train$label<-as.factor(train$label)


#Initialising H2O keeping memory at maximum of 10gb
localH2O = h2o.init(max_mem_size = '10g', nthreads = -1) 
train_h2o = as.h2o(train)
test_h2o = as.h2o(test)

#Training a  deep learning model with 2 hiddern layers of 500 nodes each
model =  h2o.deeplearning(x = 2:132,  y = 1,  training_frame = train_h2o, activation = "RectifierWithDropout", input_dropout_ratio = 0.2, hidden_dropout_ratios = c(0.5,0.5), balance_classes = TRUE, hidden = c(500,500),  momentum_stable = 0.99, nesterov_accelerated_gradient = T, epochs = 800) 
h2o.confusionMatrix(model)

#Predicting the dependent variable in the testing set
h2o_y_test <- h2o.predict(model, test_h2o)

sub = as.data.frame(h2o_y_test)
sol <- data.frame(id=test$id,loss=sub$predict)
write.csv(sol,'C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\All State Claims Severity Competition\\file_sub_DeepLearning.csv',row.names=FALSE)

#SHutting down h2o
h2o.shutdown(prompt = F)


