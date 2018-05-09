#Code Chunk1
#1.A Loading packages and data
install.packages("rmarkdown")
install.packages("scatterplot3d")
install.packages("C50")
install.packages("e1071")
install.packages("caret")
install.packages("rminer")
install.packages("matrixStats")
install.packages("knitr")
install.packages("psych")

library(rmarkdown)
library(scatterplot3d)
library(C50)
library(e1071)
library(caret)
library(rminer)
library(matrixStats)
library(knitr)
library(psych)

bart_rider<-read.csv("Assignment 1_BartRider.csv",stringsAsFactors = FALSE)

#1.B Structure and summary of data
str(bart_rider)
summary(bart_rider)

#1.C COnverting variables to factors
bart_rider$DualInc<-factor(bart_rider$DualInc)
bart_rider$Gender<-factor(bart_rider$Gender)
bart_rider$Language<-factor(bart_rider$Language)
bart_rider$OwnRent<-factor(bart_rider$OwnRent)
bart_rider$Rider<-factor(bart_rider$Rider)


str(bart_rider)
summary(bart_rider)

#1.D Number of rows and columns
nRows<-nrow(bart_rider)
nCols<-ncol(bart_rider)

#1.E first 10 and last 10 instances of data
head(bart_rider,n=10)
tail(bart_rider,n=10)

#Code Chunk2
#2.A Histograms and box plots for each of the integer variables
hist(bart_rider$Age,
     main="Histogram  of Age variable",xlab = "Age",ylab = "Count")
hist(bart_rider$DistToWork,
     main="Histogram  of DistToWork variable",xlab = "Distance to work",ylab = "Count")
hist(bart_rider$Education,
     main="Histogram  of Education variable",xlab = "Education",ylab = "Count")
hist(bart_rider$Income,
     main="Histogram  of Income variable",xlab = "Income",ylab = "Count")
hist(bart_rider$NbrInHouseHold,
     main="Histogram  of NbrInHouseHold variable",
     xlab = "Number in household",ylab = "Count")
hist(bart_rider$NbrInHouseholdUnder18,
     main="Histogram  of NbrInHouseholdUnder18 variable", 
     xlab = "Number in household under 18 years old",ylab = "Count")
hist(bart_rider$YrsInArea,main="Histogram  of YrsInArea variable",
     xlab = "Years lived in bay area",ylab = "Count")

boxplot(bart_rider$Age,
        main="Boxplot  of Age variable",xlab = "Age",ylab = "Count")
boxplot(bart_rider$DistToWork,
        main="Boxplot  of DistToWork variable",xlab = "Distance to work",ylab = "Count")
boxplot(bart_rider$Education,main="Boxplot  of Education variable",
        xlab = "Education",ylab = "Count")
boxplot(bart_rider$Income,
        main="Boxplot  of Income variable",xlab = "Income",ylab = "Count")
boxplot(bart_rider$NbrInHouseHold,
        main="Boxplot  of NbrInHouseHold variable",
        xlab = "Number in household",ylab = "Count")
boxplot(bart_rider$NbrInHouseholdUnder18,
        main="Boxplot  of NbrInHouseholdUnder18 variable",
        xlab = "Number in household under 18 years old",ylab = "Count")
boxplot(bart_rider$YrsInArea,
        main="Boxplot  of YrsInArea variable",xlab = "Years lived in bay area",ylab = "Count")

#2.B.i Mean,Variance,Standard DeviationQuantiles and dectiles
mean(bart_rider$DistToWork)
mean(bart_rider$NbrInHouseHold)
mean(bart_rider$NbrInHouseholdUnder18)

var(bart_rider$DistToWork)
var(bart_rider$NbrInHouseHold)
var(bart_rider$NbrInHouseholdUnder18)

sd(bart_rider$DistToWork)
sd(bart_rider$NbrInHouseHold)
sd(bart_rider$NbrInHouseholdUnder18)

quantile(bart_rider$DistToWork)
quantile(bart_rider$NbrInHouseHold)
quantile(bart_rider$NbrInHouseholdUnder18)

quantile(bart_rider$DistToWork,
         probs = seq(0,1,length=11),type = 5)
quantile(bart_rider$NbrInHouseHold,
         probs = seq(0,1,length=11),type = 5)
quantile(bart_rider$NbrInHouseholdUnder18,
         probs = seq(0,1,length=11),type = 5)

#2.B.ii Min-Max Normalization and Mean,Variance,
#Standard DeviationQuantiles and dectiles
n_DistToWork<-(bart_rider$DistToWork-min(bart_rider$DistToWork))/
  (max(bart_rider$DistToWork)-min(bart_rider$DistToWork))
n_NbrInHouseHold<-(bart_rider$NbrInHouseHold-min(bart_rider$NbrInHouseHold))/
  (max(bart_rider$NbrInHouseHold)-min(bart_rider$NbrInHouseHold))
n_NbrInHouseholdUnder18<-(bart_rider$NbrInHouseholdUnder18-min(bart_rider$NbrInHouseholdUnder18))/
  (max(bart_rider$NbrInHouseholdUnder18)-min(bart_rider$NbrInHouseholdUnder18))

mean(n_DistToWork)
var(n_DistToWork)
sd(n_DistToWork)
quantile(n_DistToWork)
quantile(n_DistToWork,
         probs = seq(0,1,length=11),type = 5)

mean(n_NbrInHouseHold)
var(n_NbrInHouseHold)
sd(n_NbrInHouseHold)
quantile(n_NbrInHouseHold)
quantile(n_NbrInHouseHold,
         probs = seq(0,1,length=11),type = 5)

mean(n_NbrInHouseholdUnder18)
var(n_NbrInHouseholdUnder18)
sd(n_NbrInHouseholdUnder18)
quantile(n_NbrInHouseholdUnder18)
quantile(n_NbrInHouseholdUnder18,
         probs = seq(0,1,length=11),type = 5)

#Code Chunk3
##3.A The distribution of a factor variable - For each factor variable and 
#each of this variable's nominal values, find out and show the percentage of instances that include the nominal value.
prop.table(table(bart_rider$DualInc))
prop.table(table(bart_rider$Gender))
prop.table(table(bart_rider$Language))
prop.table(table(bart_rider$OwnRent))
prop.table(table(bart_rider$Rider))

#3.B Choose two factor variables. For each factor variable, show a barplot of the number of instances (or count) 
#with a nominal value for each possible value. The bars should be arranged in descending order of instance count. 
#Show a descriptive title in each plot.
barplot(sort(table(bart_rider$DualInc),decreasing = TRUE),
        main="Count of Dual Income",xlab="Yes/No",ylab="Count")
barplot(sort(table(bart_rider$OwnRent),decreasing = TRUE),
        main="Count of Owned,Parent or Rented Property",
        xlab="Owned,Parent or Rented Property",ylab="Count")

#3.C For each of the other three factor variables, retrieve and save the number of levels. 
(levels_Gender<-levels(bart_rider$Gender))
(levels_Language<-levels(bart_rider$Language))
(levels_Rider<-levels(bart_rider$Rider))

#Code Chunk4
##4.A Use cor and pairs.panel to display correlations for all numeric variables. 
cor(bart_rider[,unlist(lapply(bart_rider,is.numeric))])
pairs.panels(bart_rider[,unlist(lapply(bart_rider,is.numeric))])

#4.B.	For DistToWork and Income,
#i	Show a boxplot of each variable by Rider and by Gender respectively.
boxplot(bart_rider$DistToWork~bart_rider$Rider,
        main="Boxplot  of DistToWork variable by Ridership",
        xlab="Ridership",ylab="Distance to work")
boxplot(bart_rider$DistToWork~bart_rider$Gender,
        main="Boxplot  of DistToWork variable by Gender",
        xlab="Gender",ylab="Distance to work")

boxplot(bart_rider$Income~bart_rider$Rider,
        main="Boxplot  of Income variable by Ridership",
        xlab="Ridership",ylab="Income")
boxplot(bart_rider$Income~bart_rider$Gender,
        main="Boxplot  of Income variable by Gender",
        xlab="Gender",ylab="Income")


##4.B.ii Use the aggregate function with summary to aggregate each variable by Rider and by Gender respectively. 
#The output should be the six number statistics of DistToWork (and separately Income) 
#aggregated by Rider's (and separately by Gender's) nominal values.
aggregate(bart_rider$DistToWork,list(Rider=bart_rider$Rider),summary)
aggregate(bart_rider$DistToWork,list(Gender=bart_rider$Gender),summary)

aggregate(bart_rider$Income,list(Rider=bart_rider$Rider),summary)
aggregate(bart_rider$Income,list(Gender=bart_rider$Gender),summary)

##4.C Draw a 3d scatter plot to show Rider values in shapes and the attributes - 
#Gender, Income and Age along the three axes
#Include a main title for the plot and legend for the Rider shapes in the plot 

scatterplot3d(bart_rider$Gender,bart_rider$Income,bart_rider$Age, pch = as.numeric(bart_rider$Rider), 
              highlight.3d = TRUE,type="p",
              main = "3D scatter plot of female titanic data",xlab="Gender",ylab="Income",zlab="Age")
legend('topleft', legend = levels(bart_rider$Rider),  pch = 1:2)

#Code Chunk5 Data partitioning and inspection code for the following:
#5.A Partition the data set for simple hold-out evaluation - 50% for training and the other 50% for testing.
set.seed(100)
inTrain <- createDataPartition(bart_rider$Rider, p=0.5, list=FALSE)
str(inTrain)

bart_rider_train<-bart_rider[inTrain,]
bart_rider_test<-bart_rider[-inTrain,]

#5.B	Show the overall structure and summary of train and test sets. 
#Show the distributions of Rider in the entire set, the train set and the test set.

str(bart_rider_train)
str(bart_rider_test)
str(bart_rider)

summary(bart_rider_train)
summary(bart_rider_test)
summary(bart_rider)

table(bart_rider_train$Rider)
table(bart_rider_test$Rider)
table(bart_rider$Rider)

prop.table(table(bart_rider_train$Rider))
prop.table(table(bart_rider_test$Rider))
prop.table(table(bart_rider$Rider))

#Code Chunk6 Simple decision tree training and testing:
#6.A	Train a C5.0 model using the default setting. Show information about this model and the summary of the model.  
#Don't need to plot the tree at this point because the tree might be too complex. 
#Generate and compare this model's confusion matrices and classification evaluation metrics in testing and training sets.
# Use the train set to build a model

train_c50 <- C5.0(Rider~., bart_rider_train)
train_c50
summary(train_c50)

# Apply the model to the hold-out test set and generate holdout evaluation metrics
predict_rider<-predict(train_c50, bart_rider_test)

# mmetric() generates confusion matrix (3rd argument) based on the true target variable values (1st argument)
# and the predicted target variable values (2nd argument) 
mmetric(bart_rider_test$Rider, predict_rider, metric="CONF")
mmetric(bart_rider_test$Rider, predict_rider, metric=c("ACC","TPR","PRECISION","F1"))

# For comparison, apply the model to the train set and generate evaluation metrics. 
# Check out the performance drop in the holdout set.
predict_train <- predict(train_c50, bart_rider_train)
mmetric(bart_rider_train$Rider, predict_train, metric="CONF")
mmetric(bart_rider_train$Rider, predict_train, metric=c("ACC","TPR","PRECISION","F1"))


#6.B Explore reducing the tree complexity by the lowering CF levels. 
#In the code, select a CF level of your choice to train and test another C5.0 model.  
#Generate and compare this model's confusion matrices and classification evaluation metrics in testing and training sets.
train_c50_v1 <- C5.0(bart_rider_train[-12],bart_rider_train$Rider, control = C5.0Control(CF = 0.05))
train_c50_v1
plot(train_c50_v1)
summary(train_c50_v1)

# Apply the model to the hold-out test set and generate holdout evaluation metrics
predict_rider_v1<-predict(train_c50_v1, bart_rider_test)

# mmetric() generates confusion matrix (3rd argument) based on the true target variable values (1st argument)
# and the predicted target variable values (2nd argument) 
mmetric(bart_rider_test$Rider, predict_rider_v1, metric="CONF")
mmetric(bart_rider_test$Rider, predict_rider_v1, metric=c("ACC","TPR","PRECISION","F1"))

# For comparison, apply the model to the train set and generate evaluation metrics. 
# Check out the performance drop in the holdout set.
predict_train_v1<- predict(train_c50_v1, bart_rider_train)
mmetric(bart_rider_train$Rider, predict_train_v1, metric="CONF")
mmetric(bart_rider_train$Rider, predict_train_v1, metric=c("ACC","TPR","PRECISION","F1"))



