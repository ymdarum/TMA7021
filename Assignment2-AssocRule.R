library(caret)
library(arules)

#load the data
dtLoc <- read.csv('./data-all-yes-no.csv')
#removing col 1.numbering and col 2. Business.Name
dtLoc2 <- dtLoc[,c(-1:-2)]

#checking for NA 
complete.cases(dtLoc2)

#remove na value
dtLoc3 <- na.omit(dtLoc2)

#removing zero varience
var.zero.dtLoc <- nearZeroVar(dtLoc3)
head(dtLoc3[,var.zero.dtLoc])
#removing Ampleparking, LRT, Hospital, Hotel, Bookshop, Printing, convenience.shop, Restaurant
dtLoc3<-dtLoc3[-var.zero.dtLoc]

#checking for the variables datatype
str(dtLoc3)
#print out the first six rows
head(dtLoc3)

summary(dtLoc3$Business.Type)
#describing the data
#determine which business type is the highest recorded
splitdtLoc3 <- split(dtLoc3$Business.Type, dtLoc3$Business.Type)
dtLoc3BizType <- sort(sapply(splitdtLoc3, length),decreasing = TRUE)

#ploting graph
barplot(dtLoc3BizType, main="Data Summary", xlab="Business Type") 
#plot(dtLoc3BizType,type="h")
#hist(dtLoc3BizType)
#write back to inspect further
#write.csv(summary(dtLoc3),"data-output.txt")


#inspect the highest count for Business Type
#dtSummary<-read.table('data-output.txt',sep=",", header=TRUE)

#Run Apriori with default setting default support = 0.1, confidence = 0.8
rules.all <- apriori(dtLoc3) 
#Error in apriori(dtLoc3) : not enough memory. Increase minimum support!
rules.all 

#i want to adjust the parameter
rhsparam <- c("Business.Type=Restaurant")
rules<-apriori(dtLoc3, control=list(verbose=F),
               parameter=list(minlen=3,maxlen=8, supp=0.009,conf=0.8),
               appearance = list(rhs=rhsparam,
                                 default="lhs"))

#with a lower minimum support, more rules will be produced, and the associations between 
#itemsets shown in the rules will be more likely to be by chance.In the above code, 
#the minimum support is set to 0.05, so each rule is supported at least by 25 (=ceiling(0.05 * 505)) cases.

#sort rules by lift
rules.sorted <- sort(rules,by="lift")
rules.sorted

#find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
#which(redundant)

#remove redundant rules
rules.pruned <- rules.sorted[!redundant]
rules.pruned

#Display the list with top ten lift value
quality(rules.pruned)<-round(quality(rules.pruned),digits=3)
rules.sorted <- sort(rules.pruned,by="lift")
rules.sorted
inspect(rules.sorted[1:10])






