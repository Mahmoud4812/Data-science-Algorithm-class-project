
##Import data to R
bank <- read.csv("C:/Users/m48ha/OneDrive/Desktop/bank.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
summary(bank)
str(bank)
data<- bank
#### Model 1 - Build Model by fitting Decision Tree Classification ####

#Split the dataset into traning and testing datase 
set.seed(254)
train_ind <- sample(1:nrow(data), 0.8 * (nrow(data)))
training_set <- data[train_ind, ]
testing_set <-data[-train_ind,]

#fit a simple classification tree model
#Fit classification tree models or rule-based models using Quinlan's C5.0 algorithm
#install.packages("C50")
library(C50)
cmodel <- C5.0(x = training_set[, -17], y = as.factor(training_set$y))

#Results of train model
summary(cmodel)

### Evaluate model performance
cmodel_pred <- predict(cmodel, testing_set)

### Cross table validation
#install.packages("gmodels")
library(gmodels)
CrossTable(testing_set$y, cmodel_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


###### Model 2 - Build Model by fitting K-means clustering ####
###Converting to numerial factors
# with two levels are transformed into a binary numerical variable
#install("dplyr")
#install.packages("car")
library(dplyr)
library(car)

bank$y <- recode(bank$y, "'no'=0; 'yes'=1", as.factor=FALSE)
bank$default <- recode(bank$default, "'no'=0; 'yes'=1", as.factor=FALSE)
bank$housing <- recode(bank$housing, "'no'=0; 'yes'=1", as.factor=FALSE)
bank$loan <- recode(bank$loan, "'no'=0; 'yes'=1", as.factor=FALSE)

# This code of chunk make the character data into numeic format
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))

str(bank)


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalize the data to get rid of outliers if present in the data set
bank_normalize <- as.data.frame(lapply(bank, normalize))

# We continue with k-means clustering. First, we need to determine the 
#number of clusters
totwinss=c()
for (i in 2:7){
  k_cl=kmeans(bank_normalize,i)
  totwinss[i] <- k_cl$tot.withinss 
}

#plot totwinss
plot(1:7, totwinss,
     xlab = "Number of clusters",
     ylab = "Within groups sum of squares")
lines(1:7, totwinss)


#we have elbow in 5 clusters
k_cl=kmeans(bank_normalize,5)

#Calculate the accuracy of the model
#chang y variable to numeric format
directions.factor<-factor(data$y)
y<-as.numeric(directions.factor)

kmean_result<-cbind(k_cl$cluster,y)
n_true_kmean<-kmean_result[which(kmean_result[,1] == kmean_result[,2]), ]
accuracy_kmean<- (nrow(n_true_kmean)/4521)*100
accuracy_kmean

#we have elbow in 3 clusters
k_cl2=kmeans(bank_normalize,3)

#Calculate the accuracy of the model
kmean_result2<-cbind(k_cl2$cluster,y)
n_true_kmean2<-kmean_result2[which(kmean_result2[,1] == kmean_result2[,2]), ]
accuracy_kmean2<- (nrow(n_true_kmean2)/4521)*100
accuracy_kmean2

#we have elbow in 2 clusters
k_cl3=kmeans(bank_normalize,2)

#Calculate the accuracy of the model
kmean_result3<-cbind(k_cl3$cluster,y)
n_true_kmean3<-kmean_result3[which(kmean_result3[,1] == kmean_result3[,2]), ]
accuracy_kmean3<- (nrow(n_true_kmean3)/4521)*100
accuracy_kmean3
