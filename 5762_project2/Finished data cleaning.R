############################
#Data Cleaning
############################

library(tidyverse)

baby <- read.delim("C:/Users/ntrac/OneDrive/Documents/IDMasters/MT5762/Project 2/babies23data.txt", sep="", header=TRUE)
attach(baby)  


#Change names to be more descriptive
colnames(baby)[7] <- "bwt"
colnames(baby)[9] <- "mrace"
colnames(baby)[10] <- "mage"
colnames(baby)[11] <- "med"
colnames(baby)[12] <- "mht" 
colnames(baby)[13] <- "mwt"



#Remove columns that don't show useful information/are all the same
baby <- baby[-grep('id', colnames(baby))] 
baby <- baby[-grep('pluralty', colnames(baby))] 
baby <- baby[-grep('outcome', colnames(baby))] 
baby <- baby[-grep('sex', colnames(baby))]


#Convert dates to actual date format
baby$date <- as.Date((date-1096), origin = "1961-01-01")


#Replace all unknowns with NA as per descriptor readme values
baby$gestation[baby$gestation == "999"] <- NA
baby$mrace[baby$mrace == "99"] <- NA
baby$mage[baby$mage == "99"] <- NA
baby$med[baby$med == "9"] <- NA
baby$mht[baby$mht == "99"] <- NA
baby$mwt[baby$mwt == "999"] <- NA
baby$drace[baby$drace == "99"] <- NA
baby$dage[baby$dage == "99"] <- NA
baby$ded[baby$ded == "9"] <- NA
baby$dht[baby$dht == "99"] <- NA
baby$dwt[baby$dwt == "999"] <- NA
baby$marital[baby$marital == "0"] <- NA
baby$inc[baby$inc == "98"] <- NA
baby$smoke[baby$smoke == "9"] <- NA
baby$time[baby$time == "99"] <- NA
baby$time[baby$time == "98"] <- NA  
baby$number[baby$number == "98"] <- NA
baby$number[baby$number == "9"] <- NA  


#Combine values which are the same
baby$mrace[baby$mrace == 0] <- 5
baby$mrace[baby$mrace == 1] <- 5
baby$mrace[baby$mrace == 2] <- 5
baby$mrace[baby$mrace == 3] <- 5
baby$mrace[baby$mrace == 4] <- 5
baby$med[baby$med == 7] <- 6
baby$drace[baby$drace == 0] <- 5
baby$drace[baby$drace == 1] <- 5
baby$drace[baby$drace == 2] <- 5
baby$drace[baby$drace == 3] <- 5
baby$drace[baby$drace == 4] <- 5
baby$ded[baby$ded == 7] <- 6


############################
#Test Set
############################
#get a random number so my random number generator is deterministic.
set.seed(095)

#The test set baby weights.
test_set <- as.data.frame(sample(unique(baby[, 1]) , round((length(baby[, 1])*0.2))))
colnames(test_set) <- "id"

#TRAINING
#get training dataset
training_set<-as.data.frame(unique(baby[, 1])[which(is.na(match(unique(baby[, 1]), test_set[, 1])))])
colnames(training_set) <- "id"

#check it worked.
nrow(training_set) + nrow(test_set) == length(unique(baby[, 1]))

#Analysis data
schooling <- semi_join(baby, training_set, by = "id")

#save test data for after the model is built. 
test_data <- semi_join(baby, test_set, by = "id")

#check no child left behind.
nrow(test_data) + nrow(training_set) == nrow(baby)



