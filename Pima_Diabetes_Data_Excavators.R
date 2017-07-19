#Team - Data Excavators
#Dataset - Pima Indian Diabetes

#Imputation of missing values
#Pima Indians Dataset - Insulin, Glucose, SkinThickness, BloodPressure, BMI have values that are
#missing, i.e., 0. Therefore, before we can proceed any further we have to predict the missing values
#as having no values for the missing cells will cause problems during further evaluation.

#mice package is used to impute missing values
library(mice)

data <-read.csv(file="Indians_Diabetes_Database.csv",stringsAsFactors=FALSE,header=TRUE)
outcome <- data$Outcome
#removing the outcome column
data <- data[,-9]

#replace missing values in col 4 and 5 skin thickness and insulin with NA

#Replacing the missing values in 2,3,4,5,6 columns with NA.
data[, 2:6][data[, 2:6] == 0] <- NA
View(data)


#% of missing data
percentage_missing <- function(x){sum(is.na(x))/length(x)*100}
percentage_missing

#shows columns and percent missing
apply(data,2,percentage_missing)


#plots
md.pattern(data)


library(VIM)

#Histogram of missing data
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#sample random sample from the observed values
#m is number of multiple imputations default is 5, maxit is number of iterations
#with default equal to 5, seed is for offsetting the random number generator
tempData <- mice(data,m=5,maxit=5,meth='norm.predict',seed=500)
?mice
summary(tempData)

#now check the imputed data
tempData$imp$Insulin
tempData$imp$SkinThickness
tempData$imp$Glucose
tempData$imp$BloodPressure
tempData$imp$BMI

#now put data back together
completedData <- complete(tempData,1)
View(completedData)

library(lattice)
#magenta is imputed
#blue is observed
densityplot(tempData)

#stripplot shows individual data points
stripplot(tempData, pch = 20, cex = 1.2)

#Our data mining tasks are first to understand the domain and data
#(data understanding) and prepare the data (this utilizes our knowledge of databases)
#and problem or opportunity
#then we need to learn the data and refine our problem or opportunity
#and discover knowledge


###################################################
### preliminary exploration of data part 1
###################################################
dim(completedData)
names(completedData)
str(completedData)
attributes(completedData)


###################################################
### preliminary exploration of data part 2
###################################################
completedData[1:5,]
head(completedData)
tail(completedData)


###################################################
### preliminary exploration of data part 3
###################################################
completedData[1:10, "Insulin"]


###################################################
### preliminary exploration of data part 4
###################################################
summary(completedData)


###################################################
### preliminary exploration of data part 5
###################################################
quantile(completedData$Insulin)

###################################################
####### preliminary exploration of data part 6
###################################################
var(completedData$Insulin)
hist(completedData$Insulin)



###################################################
### preliminary exploration of data part 7
###################################################
plot(density(completedData$Insulin))


###################################################
### preliminary exploration of data part 8
###################################################
table(completedData$Pregnancies)
pie(table(completedData$Pregnancies))


###################################################
### preliminary exploration of data part 9
###################################################
barplot(table(completedData$Pregnancies))


###################################################
### preliminary exploration of data part 10
#note correlation coefficient r
#0 is no correlation
#1 is perfectly positive correlation
#-1 is perfectly negative correlation

###################################################
cov(completedData$Glucose, completedData$Insulin)

cor(completedData$Glucose, completedData$Insulin)
cor(completedData$Pregnancies, completedData$Insulin)



###################################################
### preliminary exploration of data part 11
###################################################
aggregate(completedData$SkinThickness ~ completedData$Insulin, summary, data=completedData)


###################################################
### preliminary exploration of data part 12
###################################################
boxplot(completedData$SkinThickness ~ completedData$Insulin, data=completedData)


#now a box plot
#red shows distribution of pregnancies with insulin
#missing
marginplot(completedData[c(1,5)])
#another box plot
#red shows distribution of pregnancies with skin thickness
#missing
marginplot(completedData[c(1,4)])
#
#another box plot
#red shows distribution of pregnancies with BMI
#missing
marginplot(completedData[c(1,6)])

#another box plot
#red shows distribution of pregnancies with Blood Pressure
#missing
marginplot(completedData[c(1,3)])

#Outliers in data can distort predictions and affect the accuracy, 
#if you don't detect and handle them appropriately.

boxplot(completedData$Glucose~completedData$BloodPressure,data=completedData,main="Distribution of diabetes_set Glucose vs Blood Pressure", xlab="Blood Pressure" , ylab = "Glucose" , col="red")
boxplot(completedData$Glucose~completedData$BMI,data=completedData,main="Distribution of diabetes_set Glucose vs BMI", xlab="BMI" , ylab = "Glucose" , col="blue")
boxplot(completedData$Glucose~completedData$Insulin,data=completedData,main="Distribution of diabetes_set Glucose vs Insulin", xlab="Insulin" , ylab = "Glucose" , col="yellow")
boxplot(completedData$Glucose~completedData$SkinThickness,data=completedData,main="Distribution of diabetes_set Glucose vs skinThickness", xlab="skinThickness" , ylab = "Glucose" , col="green")

# Removing outliers is no solution as it impacts adversely  on the sensitivity of
# the data, therefore lets do the transformation on numerical variables which
# impacts glucose levels directly or indirectly.
# min max normalization to normalize skinthickness, insulin, BMI, Blood Pressure and Glucose.

# skinThickness
completedData$SkinThickness_transform<-(completedData$SkinThickness-min(completedData$SkinThickness))/(max(completedData$SkinThickness)-min(completedData$SkinThickness))

#Insulin
completedData$Insulin_transform<-(completedData$Insulin-min(completedData$Insulin))/(max(completedData$Insulin)-min(completedData$Insulin))

#Blood Pressure
completedData$BloodPressure_transform<-(completedData$BloodPressure-min(completedData$BloodPressure))/(max(completedData$BloodPressure)-min(completedData$BloodPressure))

#BMI
completedData$BMI_transform<- (completedData$BMI-min(completedData$BMI))/(max(completedData$BMI)-min(completedData$BMI))

#Glucose
completedData$Glucose_transform<- (completedData$Glucose-min(completedData$Glucose))/(max(completedData$Glucose)-min(completedData$Glucose))


n <- nrow(completedData)
labels <- 1:n
biplot(prcomp(completedData), cex=.8, xlabs=labels)

library(ggplot2)

#to find the correlation between different attributes
cor_dia = cor(completedData,use="complete.obs",method = "pearson")
cor_dia
library(corrplot)
corrplot(cor_dia,method="circle")

#Principal component analysis
library(psych)
pc1 <- principal(completedData, nfactors = 8, rotate="none")
pc1$values
plot(pc1$values,type = "b",ylab=c("variances"))
pc1


#creating test and training sets
#Partition the data into 75% training data and 25% testing data
completedData <- cbind(completedData,outcome)
modellingData <- completedData[,-(9:12)]
modellingData[9]<-lapply(modellingData[9],as.factor)
str(modellingData)
set.seed(1)
sample <- sample.int(nrow(modellingData), floor(.75*nrow(modellingData)), replace = F)
train1 <- modellingData[sample, ]
test1 <- modellingData[-sample, ]

####################################################################
#####################     CLASSIFICATION   #########################
####################################################################


######  Decision Tree modelling  ###############################
library(tree)
tree_model = tree(Outcome~., train1)
plot(tree_model)
text(tree_model,pretty = 0)
library(rpart)
library(rpart.plot)
dtm <- rpart(Outcome~., train1, method = "class")
rpart.plot(dtm)
p<-predict(dtm, test1,type="class")
tdtm<-table(test1[,9],p)
sum(diag(tdtm))/sum(tdtm)

#######  Random Forest modelling  ##############################
library(randomForest)

###to find best mtry
bestmtry <- tuneRF(train1,train1$Outcome, ntreeTry = 700,stepFactor = 1.2,improve = 0.01
                   ,trace = T, plot = T)
rfm <- randomForest(Outcome~.,train1,mtry = 3,ntree=700)
rfm
importance(rfm)
varImpPlot(rfm)
###Predict with class
predictWithClass <- predict(rfm, test1, type = "class")
predictWithClass
trfm <- table(predictions = predictWithClass, actual = test1$Outcome)
trfm
sum(diag(trfm))/sum(trfm)
###Predict with prob
library(pROC)
predictWithProb <- predict(rfm,test1,type = "prob")
predictWithProb
areaUC <- auc(test1$Outcome, predictWithProb[,2])
areaUC
plot(roc(test1$Outcome, predictWithProb[,2]))

#######  KNN  #################################
normalize <- function(x) { return ((x- min(x))/(max(x)-min(x)))}
normalized_data <- as.data.frame(lapply(modellingData[,(1:8)],normalize))
str(normalized_data)
set.seed(12)
## creating training and testing data after removing target variable
sample_normal <- sample.int(nrow(normalized_data), floor(.75*nrow(normalized_data)), replace = F)
train_normal <- normalized_data[sample_normal, ]
test_normal <- normalized_data[-sample_normal, ]
## dividing the target variable into test and train sets
train_target <- outcome[sample_normal,]
test_target <- outcome[-sample_normal,]
require(class)
dim(modellingData)
sqrt(nrow(modellingData))
knnModel <- knn(train = train_normal,test = test_normal, cl = train_target,k=27)
knnModel
t<-table(test_target,knnModel)
## find the accuracy
sum(diag(t))/sum(t)


####################################################################
#####################     CLUSTERING   #############################
####################################################################


### kmeans clustering
#standardizing variables
modellingData_stand <- scale(modellingData[-9])
k.mean.fit <- kmeans(modellingData_stand,2)
attributes(k.mean.fit)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(modellingData_stand, nc=20) 
library(cluster)
clusplot(modellingData_stand, k.mean.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=1, lines=0)


#clustering
install.packages("cluster") 
library(cluster)

agn_complete <- agnes(completedData, diss=FALSE, stand=FALSE, method ="complete") 
# Make and plot the dendrogram 
dend_agn_complete <- as.dendrogram(agn_complete)
plot(dend_agn_complete, xlab="Index of Data Points", ylab="Steps", main="Complete-LinkageClustering")

# Apply kmeans to train, and store the clustering result in kc. 
# The cluster number is set to 3.
(kc <- kmeans(completedData, 3)) 

set.seed(4)
km2 = kmeans(train, 2, nstart=13)
km2

# Plot the clusters and their centres. Note that there are four dimensions
# in the data and that only the first two dimensions are used to draw the
# plot below.

plot(completedData, col =(km2$cluste) , main="K-Means clustering of completedData set")
points(kc$centers[c("train")], col=1:3, pch=8, cex=2)

plot(completedData$Glucose,completedData$BloodPressure, col =(km2$cluster) , main="K-Means result with 2 clusters")
points(kc$centers[c("completedData.Glucose","completedData.BloodPressure")], col=1:3, pch=8, cex=2)

plot(completedData$SkinThickness,completedData$Insulin,col =(km2$cluster) , main="K-Means result with 2 clusters")
points(kc$centers[c("completedData.SkinThickness","completedData.SkinThickness")], col=1:3, pch=8, cex=2)

boxplot(completedData$DiabetesPedigreeFunction, completedData$Glucose, data=ToothGrowth, notch=TRUE,
        col=(c("blue","yellow")),
        main="Test result for Diabetes vs Pedigree Function", xlab="Test result for Diabetes" , ylab="Pedigree Function")

#Neural Network
#We had eliminated the predictor varible till now. Now wea re including it in our analysis.


#creating test and training sets
#Partition the data into 75% training data and 25% testing data
sample <- sample.int(nrow(completedData), floor(.75*nrow(completedData)), replace = F)
train <- completedData[sample, ]
test <- completedData[-sample, ]
view(train)
view(test)

#Library required for creating Neural Network
library(neuralnet)

#Creating a neural network with 4 input layers, 2 hidden layers containing
#5 neurons and 3 neurons each and 1 output layer. We have set linear.output to TRUE because we
#want linear regression and not classification. 
neunet <- neuralnet(Outcome ~ SkinThickness_tranform + Glucose_transform + Insulin_transform + BMI_transform
                    , data = train, hidden = c(5,3), linear.output = T)

plot(neunet)

#The temp dataset contains only the columns "SkinThickness_tranform", "Glucose_transform",
#"Insulin_transform", "BMI_transform" of the training data set.
#Only these variables are used for input.
temp_test <- subset(test, select = c("SkinThickness_transform", "Glucose_transform", "Insulin_transform", 
                             "BMI_transform"))
temp_test


#Computation of Output variable based on required input variables.
neunet.results <- compute(neunet, temp_test)

#Rounding off the result values to understand the results better.
neunet.results$net.result <- sapply(neunet.results$net.result,round,digits=0)

#Comparison between actual Output values from our dataset and predicted Output values
#using the Neural Network Algorithm.
results <- data.frame(actual = test$Outcome, prediction = neunet.results$net.result)

results

#Confusuion matrix for the Outcome values.
table(test$Outcome,neunet.results$net.result)




  Preg_Diabetec_Yes <- subset(completedData, completedData$outcome == 1, select = Pregnancies)
  Preg_Diabetec_Yes
  
  Preg_Diabetec_No <- subset(completedData, completedData$outcome == 0, select = Pregnancies)
  Preg_Diabetec_No
  
  t.test(Preg_Diabetec_Yes, Preg_Diabetec_No, alternative = c("greater"), var.equal=FALSE, paired=FALSE)
  

#Boxplots
  
  boxplot(Pregnancies~outcome,data=completedData, main="Pregnancies v Diabetes", 
          xlab="outcome", ylab="Number of pregnancies", col = c('yellow','red'))

  boxplot(Glucose~outcome,data=completedData, main="Glucose v Diabetes", 
          xlab="outcome", ylab="Glucose", col = c('yellow','red'))
  
  boxplot(BloodPressure~outcome,data=completedData, main="BloodPressure v Diabetes", 
          xlab="outcome", ylab="BloodPressure", col = c('yellow','red'))
  
  boxplot(Insulin~outcome,data=completedData, main="Insulin v Diabetes", 
          xlab="outcome", ylab="Insulin", col = c('yellow','red'))
  
  boxplot(SkinThickness~outcome,data=completedData, main="SkinThickness v Diabetes", 
          xlab="outcome", ylab="SkinThickness", col = c('yellow','red'))
  
  boxplot(DiabetesPedigreeFunction~outcome,data=completedData, main="DiabetesPedigreeFunction
          v Diabetes", 
          xlab="outcome", ylab="DiabetesPedigreeFunction", col = c('yellow','red'))
  
  boxplot(Age~outcome,data=completedData, main="Age v Diabetes", 
          xlab="outcome", ylab="Age", col = c('yellow','red'))
  