#removing previous data
rm(list=ls())

library(caret)
library(arules)

#load the data
dtLoc <- read.csv('data-all-yes-no.csv')
#removing col 1.numbering and col 2. Business.Name
dtLoc2 <- dtLoc[,c(-1:-2)]

#remove na value
dtLoc3 <- na.omit(dtLoc2)

#removing zero varience
var.zero.dtLoc <- nearZeroVar(dtLoc3)
head(dtLoc3[,var.zero.dtLoc])
#removing column Ampleparking, LRT, Hospital, Hotel, Bookshop, Printing, convenience.shop, Restaurant
dtLoc3<-dtLoc3[-var.zero.dtLoc]

#checking for the variables datatype
str(dtLoc3)

# define an 80%/20% train/test split of the dataset
indxTrain <- createDataPartition(y =dtLoc3$Business.Type, p = 0.80,list = FALSE)
training <- dtLoc3[indxTrain,]
testing <- dtLoc3[-indxTrain,]

#Check Data Distribution
prop.table(table(training$Business.Type)) * 100
prop.table(table(testing$Business.Type)) * 100

#Perform Cross Validation on Training
ctrl <- trainControl(method= "repeatedcv",number=10, repeats = 3)

#load library
library(klaR)
library(MASS)

# train the model 
nbFit <- train(Business.Type ~ ., data = training, method = "nb", trControl = ctrl, 
               preProcess =c("center","scale"))
#error - Something is wrong; all the Accuracy metric values are missing:
#Error in train.default(x, y, weights = w, ...) : Stopping
#In addition: There were 50 or more warnings (use warnings() to see the first 50)
#nbfit cannot be use
nbFit

#using other method
library(e1071)
# we are training the model by selecting the characteristics as determinant and Business Type as the predictor
#nb_model <- naiveBayes(facing.main.road ~ Business.Type, training)
nb_model <- naiveBayes(CornerShopLot ~ Business.Type, training)
nb_model

#we predict the model using the testing data without the column determinant
#nb_test_predict <- predict(nb_model,testing[,"facing.main.road"])
nb_test_predict <- predict(nb_model,testing)

#putting inside a confusion table with testing data
#table(pred=nb_test_predict,true=testing$facing.main.road)
table(pred=nb_test_predict,true=testing$CornerShopLot)

#confusion matrix against the testing data
#confusionMatrix(nb_test_predict, testing$facing.main.road)
confusionMatrix(nb_test_predict, testing$CornerShopLot)


