#removing previous data
rm(list=ls())

library(caret)
library(party)
library(arules)
library(rpart.plot)

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

# define an 80%/20% train/test split of the dataset
indxTrain <- createDataPartition(y =dtLoc3$Business.Type, p = 0.80,list = FALSE)
training <- dtLoc3[indxTrain,]
testing <- dtLoc3[-indxTrain,]

# prepare training scheme
ctrl <- trainControl(method="cv", number=10,repeats = 3)
# train the model
#predictor/independent variables = facing.main.road + CornerShopLot + Car.Accessories + Bank
#dependant/response variables = Business.Type
head(testing)
rpartFit <- train(Business.Type ~ facing.main.road + CornerShopLot + Car.Accessories + Bank, 
                  data =training, method = "rpart", trControl = ctrl,
                  preProcess = c("center","scale"))
rpartFit
prp(rpartFit$finalModel, main="Decision Tree on BUsiness Type",faclen = 0, cex = 0.8)

#below code are using other method and commented out
#myFormula <- Business.Type ~ facing.main.road + CornerShopLot + Car.Accessories + Bank
#dtLoc3_rpart <- rpart(myFormula, data = training, control = rpart.control(minsplit = 10))
#attributes(dtLoc3_rpart)
#print(dtLoc3_rpart)
#plot(dtLoc3_rpart)

