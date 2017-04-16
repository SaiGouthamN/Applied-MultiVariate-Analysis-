library(party)
library(sqldf)
library(plyr)
library(dplyr)

#Importing training and test data and not converting strings(characters) to factors
trainData<- read.csv("C:/Users/sai-g/Desktop/Goutham/3.App.Multivariate Analysis/Project/titanic_train.csv", header = TRUE, stringsAsFactors = FALSE,na.strings = c('NA',""," ","NULL","<NA>"))
testData<- read.csv("C:/Users/sai-g/Desktop/Goutham/3.App.Multivariate Analysis/Project/titanic_test.csv", header = TRUE, stringsAsFactors = FALSE,na.strings = c('NA',""," ","NULL","<NA>"))

#Exploration of test and training data
str(trainData)
str(testData)


#Find out the number of missing values in each field
sum(is.na(trainData$PassengerId))
sum(is.na(trainData$Pclass))
sum(is.na(trainData$Name))
sum(is.na(trainData$Sex))
sum(is.na(trainData$Age))
sum(is.na(trainData$SibSp))
sum(is.na(trainData$Parch))
sum(is.na(trainData$Cabin))
sum(is.na(trainData$Embarked))
sum(is.na(trainData$Ticket))
sum(is.na(trainData$Fare))



#Split the Name into First Name ,Last Name and Title
trainData<-separate(trainData,Name,into=c("LastName","FirstName"),sep=",",extra='merge')
trainData<-separate(trainData,FirstName,into=c("Title","FirstName"),sep=".",extra='merge')
trainData<-separate(trainData,FirstName,into=c("Title","FirstName"),sep="\\.",extra='merge')
trainData[[5]]<-NULL
head(trainData)

testData<-separate(testData,Name,into=c("LastName","FirstName"),sep=",",extra='merge')
testData<-separate(testData,FirstName,into=c("Title","FirstName"),sep=".",extra='merge')
testData<-separate(testData,FirstName,into=c("Title","FirstName"),sep="\\.",extra='merge')
testData[[4]]<-NULL
head(testData)

#Remove the unwanted columns from the testData
PassengerId = testData[1]
testData = testData[-c(1, 8:11)]
str(testData)


#Finding out the different Title values uniquely
unique(trainData$Title)


#Find average age Title wise
Title_Wise_Age<-sqldf("select Title,avg(Age) as Age from trainData group by Title")
Title_Wise_Age=as.data.frame(Title_Wise_Age)



#Fill missing Age values with the corresponding Title_Wise_Age depending on the Title
trainData <- within(trainData, Age[is.na(Age) & Title == 'Capt'] <- Title_Wise_Age$Age[[1]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Col'] <- Title_Wise_Age$Age[[2]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Don'] <- Title_Wise_Age$Age[[3]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Dr'] <- Title_Wise_Age$Age[[4]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Jonkheer'] <- Title_Wise_Age$Age[[5]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Lady'] <- Title_Wise_Age$Age[[6]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Major'] <- Title_Wise_Age$Age[[7]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Master'] <- Title_Wise_Age$Age[[8]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Miss'] <- Title_Wise_Age$Age[[9]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Mlle'] <- Title_Wise_Age$Age[[10]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Mme'] <- Title_Wise_Age$Age[[11]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Mr'] <- Title_Wise_Age$Age[[12]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Mrs'] <- Title_Wise_Age$Age[[13]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Ms'] <- Title_Wise_Age$Age[[14]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Rev'] <- Title_Wise_Age$Age[[15]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'Sir'] <- Title_Wise_Age$Age[[16]])
trainData <- within(trainData, Age[is.na(Age) & Title == 'the Countess'] <- Title_Wise_Age$Age[[17]])
head(trainData,30)




#Fill missing Age values with the corresponding Title_Wise_Age depending on the Title
testData <- within(testData, Age[is.na(Age) & Title == 'Capt'] <- Title_Wise_Age$Age[[1]])
testData <- within(testData, Age[is.na(Age) & Title == 'Col'] <- Title_Wise_Age$Age[[2]])
testData <- within(testData, Age[is.na(Age) & Title == 'Don'] <- Title_Wise_Age$Age[[3]])
testData <- within(testData, Age[is.na(Age) & Title == 'Dr'] <- Title_Wise_Age$Age[[4]])
testData <- within(testData, Age[is.na(Age) & Title == 'Jonkheer'] <- Title_Wise_Age$Age[[5]])
testData <- within(testData, Age[is.na(Age) & Title == 'Lady'] <- Title_Wise_Age$Age[[6]])
testData <- within(testData, Age[is.na(Age) & Title == 'Major'] <- Title_Wise_Age$Age[[7]])
testData <- within(testData, Age[is.na(Age) & Title == 'Master'] <- Title_Wise_Age$Age[[8]])
testData <- within(testData, Age[is.na(Age) & Title == 'Miss'] <- Title_Wise_Age$Age[[9]])
testData <- within(testData, Age[is.na(Age) & Title == 'Mlle'] <- Title_Wise_Age$Age[[10]])
testData <- within(testData, Age[is.na(Age) & Title == 'Mme'] <- Title_Wise_Age$Age[[11]])
testData <- within(testData, Age[is.na(Age) & Title == 'Mr'] <- Title_Wise_Age$Age[[12]])
testData <- within(testData, Age[is.na(Age) & Title == 'Mrs'] <- Title_Wise_Age$Age[[13]])
testData <- within(testData, Age[is.na(Age) & Title == 'Ms'] <- Title_Wise_Age$Age[[14]])
testData <- within(testData, Age[is.na(Age) & Title == 'Rev'] <- Title_Wise_Age$Age[[15]])
testData <- within(testData, Age[is.na(Age) & Title == 'Sir'] <- Title_Wise_Age$Age[[16]])
testData <- within(testData, Age[is.na(Age) & Title == 'the Countess'] <- Title_Wise_Age$Age[[17]])
head(trainData,30)


#Creating a new field named "Family"
trainData['Family']<-0
testData['Family']<-0



#Assign 1 to the family field if the SubSP is 1 or assign 0
trainData <- within(trainData, Family[SibSp>=1] <- 1)
trainData <- within(trainData, Family[SibSp<1] <- 0)
str(trainData)


#Assign 1 if Sex is male and 0 if sex is female
trainData <- within(trainData, Sex[Sex=='male'] <- 1)
trainData <- within(trainData, Sex[Sex=='female'] <- 0)
testData <- within(testData, Sex[Sex=='male'] <- 1)
testData <- within(testData, Sex[Sex=='female'] <- 0)
trainData$Sex=as.numeric(trainData$Sex)
testData$Sex=as.numeric(testData$Sex)


#Apply te ctree model to predict the independent variable Survived
my_tree <- ctree(as.factor(Survived) ~ Pclass + Pclass*Sex  + Age*Sex + Family ,
                 data = trainData)

# Make your prediction using the test set
survival <- predict(my_tree, testData)


#Submit the values to the output file
kaggle.sub <- cbind(PassengerId,survival)
colnames(kaggle.sub) <- c("PassengerId", "Survived")
write.csv(kaggle.sub, file = "C:\\Users\\sai-g\\Desktop\\Goutham\\3.App.Multivariate Analysis\\Project\\Kaggle_CTree_2.csv", row.names = FALSE)

