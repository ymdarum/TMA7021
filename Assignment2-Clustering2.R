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
#removing the Business Type  column
dt.extract <- dtLoc3[,-1]

#data transform from yes and no to 1 and 0
for (i in 1:length(dt.extract)) {
    dt.extract[,i] <- gsub("yes", "1", dt.extract[,i])
    dt.extract[,i] <- gsub("no", "0", dt.extract[,i])
}

#convert to numeric
for (i in 1:length(dt.extract)) {
  dt.extract[,i] <- sapply(dt.extract[,i], as.numeric) # or as.integer
}
#str(dt.extract)
#summary(dt.extract)
#checking for NA 
#complete.cases(dt.extract)

dist(dt.extract, method = "euclidean")
dist(dt.extract, method = "manhattan")

kmeansFit <- kmeans(dt.extract, 4)
attributes(dt.extract)

kmeansFit$centers
kmeansFit$cluster

wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type = "b", xlab = "Number of CLusters", ylab = "Within groups sum of squares")
}

#checking for suitable nmber of cluster
wssplot(dt.extract, nc = 10)


library(cluster)
table(kmeansFit$cluster)
clusplot(dt.extract,
         kmeansFit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)

#Dendrogram
d <- dist(dt.extract, method = "euclidean")
H.fit <- hclust(d, method="ward.D2")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=4)
rect.hclust(H.fit, k=4, border="red")