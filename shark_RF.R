install.packages("randomForest", dependencies=TRUE) #Random Forest Model
install.packages("caret", dependencies=TRUE)
install.packages('e1071', dependencies=TRUE)
install.packages('ROCR', dependencies=TRUE)
install.packages('nnet', dependencies=TRUE) #Neural Net Model
install.packages('mice', dependencies=TRUE) #Imputation

library(mice)
library(nnet)
library(randomForest)
library(caret)
library(dplyr)
library(data.table)


setwd("C:/Meetu/AdvancedBI/groupProject")

mydata <- read.csv(file="sharks.csv", header=T, sep=",", strip.white = T, na.strings = c("NA","NaN","","?"))
summary(mydata)

colnames(mydata)
ncol(mydata)
attack_levels <- data.frame(mydata[c(18)])

###   Classification Levels   ###
### Level Based Columns
mydata_levels <- data.frame(mydata[c(8,11,49,38:44,46,51,53)])
impute_data <- mice(mydata_levels[1:13], meth="pmm")
mydata_levels <- complete(impute_data)

mydata_levels <- data.frame(c(attack_levels, mydata_levels)) #Add Attack Column)

summary(mydata_levels)
colnames(mydata_levels)

###  Value Based Columns
mydata_values <- data.frame(mydata[c(10,30,27,28,29,24,25,26,45,51,53)])
impute_data <- mice(mydata_values[ ,1:11], meth="pmm")
mydata_values <- complete(impute_data)

mydata_values <- data.frame(c(attack_levels, mydata_values)) #Add Attack Column



#Select Data Type: Values or Levels
#mydata <- mydata_values  ### values based data
mydata <- mydata_levels   ### levels based data
summary(mydata)

##Convert the following variables into factor variables using function as.factor()
#mydata$TurtleExactCombined <- as.factor(mydata$TurtleExactCombined)
#mydata$DissovedO2 <- as.factor(mydata$DissovedO2)
#mydata$Salinity <- as.factor(mydata$Salinity)
#mydata$Turbidity <- as.factor(mydata$Turbidity)
#mydata$Temperature <- as.factor(mydata$Temperature)
#mydata$Precipitation_Value <- as.factor(mydata$Precipitation_Value)
#mydata$StationPressure <- as.factor(mydata$StationPressure)
#mydata$WindSpeed <- as.factor(mydata$WindSpeed)
#mydata$prepmovingaverage <- as.factor(mydata$prepmovingaverage)
#mydata$Direction <- as.factor(mydata$Direction)
#mydata$MoonPhase4daysextended <- as.factor(mydata$MoonPhase4daysextended) 

##Convert the following variables into factor variables using function as.factor()
mydata$turtleexactdiscretizeSC <- as.factor(mydata$turtleexactdiscretizeSC)
mydata$TurtleexactdiscretizeNC <- as.factor(mydata$TurtleexactdiscretizeNC)
mydata$CrabLandingsDisc <- as.factor(mydata$CrabLandingsDisc)
mydata$DissolvedO2discretize <- as.factor(mydata$DissolvedO2discretize)
mydata$salinitydiscretize <- as.factor(mydata$salinitydiscretize)
mydata$turbiditydiscretize <- as.factor(mydata$turbiditydiscretize)
mydata$temperaturediscretize <- as.factor(mydata$temperaturediscretize)
mydata$precipitationdiscretize <- as.factor(mydata$precipitationdiscretize)
mydata$pressurediscretize <- as.factor(mydata$pressurediscretize)
mydata$windspeeddiscretize <- as.factor(mydata$windspeeddiscretize)
mydata$precipitationmvadiscretize <- as.factor(mydata$precipitationmvadiscretize)
mydata$Direction <- as.factor(mydata$Direction)
mydata$MoonPhase4daysextended <-  as.factor(mydata$MoonPhase4daysextended)
mydata$Attack <-  as.factor(mydata$Attack)

## Remove Missing Values using complete.cases:
mydata <- mydata[complete.cases(mydata), ]  # We only keep the observations with no missing values.
summary(mydata)

## Create the Training data set with 70% data and Test Data with 30% data:
# n will be ther number of obs. in data
n = nrow(mydata)
print(n)
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train_data = mydata[trainIndex,] # We use the index to create training data
test_data = mydata[-trainIndex,] # We take the remaining 30% as the testing data
n1 = nrow(train_data)
print(n1)
n2 = nrow(test_data)

summary(train_data)
ncol(train_data)

summary(test_data)
## Build the RF Classifier:
rf1 <- randomForest(Attack~., data = train_data, ntree=10,  na.action=na.exclude, importance=T) 
print(rf1)

rf2 <- randomForest(Attack~., data = train_data, ntree=20,  na.action=na.exclude, importance=T) 
print(rf2)

rf3<- randomForest(Attack~., data = train_data, ntree=30,  na.action=na.exclude, importance=T) 
print(rf3)

rf4 <- randomForest(Attack~., data = train_data, ntree=40,  na.action=na.exclude, importance=T) 
print(rf4)

rf5 <- randomForest(Attack~., data = train_data, ntree=50,  na.action=na.exclude, importance=T) 
print(rf5)

rf6 <- randomForest(Attack~., data = train_data, ntree=60,  na.action=na.exclude, importance=T) 
print(rf6)

rf7 <- randomForest(Attack~., data = train_data, ntree=70,  na.action=na.exclude, importance=T) 
print(rf7)

rf8 <- randomForest(Attack~., data = train_data, ntree=80,  na.action=na.exclude, importance=T) 
print(rf8)

rf9 <- randomForest(Attack~., data = train_data, ntree=90,  na.action=na.exclude, importance=T) 
print(rf9)

rf10 <- randomForest(Attack~., data = train_data, ntree=100,  na.action=na.exclude, importance=T) 
print(rf10)

rf11 <- randomForest(Attack~., data = train_data, ntree=110,  na.action=na.exclude, importance=T) 
print(rf11)

rf12 <- randomForest(Attack~., data = train_data, ntree=120,  na.action=na.exclude, importance=T) 
print(rf12)

rf13 <- randomForest(Attack~., data = train_data, ntree=130,  na.action=na.exclude, importance=T) 
print(rf13)


rf14 <- randomForest(Attack~., data = train_data, ntree=140,  na.action=na.exclude, importance=T) 
print(rf14)


rf15 <- randomForest(Attack~., data = train_data, ntree=150,  na.action=na.exclude, importance=T) 
print(rf15)

rf16 <- randomForest(Attack~., data = train_data, ntree=160,  na.action=na.exclude, importance=T) 
print(rf16)


rf17 <- randomForest(Attack~., data = train_data, ntree=170,  na.action=na.exclude, importance=T) 
print(rf17)


rf18 <- randomForest(Attack~., data = train_data, ntree=180,  na.action=na.exclude, importance=T) 
print(rf18)


rf19 <- randomForest(Attack~., data = train_data, ntree=190,  na.action=na.exclude, importance=T) 
print(rf19)


rf20 <- randomForest(Attack~., data = train_data, ntree=200,  na.action=na.exclude, importance=T) 
print(rf20)

#finetuning the RF - lowest OOB of 16.15% @ntree= 150 - value based data
##finetuning the RF - lowest OOB of 17.69% @ntree= 80 - Level based data
mtry <- tuneRF(train_data[-1], y=as.factor(train_data$Attack), ntreeTry=80,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, na.action=na.exclude)


#Final Random Forest
rf <- randomForest(Attack~., data = train_data, ntree=80, mtry = 2,  na.action=na.exclude, importance=T) 
print(rf)


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)

#Creating Confusion Matrix for RF
predicted_values <- predict(rf, test_data,type= "prob")
head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,1] > threshold, 'Yes', 'No') )
levels(test_data$Attack)[2]

confusionMatrix(pred, test_data$Attack, positive = levels(test_data$Attack)[2])


## ROC Curve for the fine tuned RF:
install.packages('caret', dependencies = TRUE)
library(caret)
library(ROCR)
predicted_values <- predict(rf, test_data, type= "prob") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,2], test_data$Attack)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="Random Forest")


ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

# Plotting variable importance 
varImpPlot(rf)

