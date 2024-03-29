############################
#Data Cleaning
############################

library(tidyverse)

get_data <- function(name) {

  baby <- read.delim(name, sep="", header=TRUE)

  #check for na values
  any(which(is.na(baby)))
  #There are no na value

  #check for missing values
  complete_c <- complete.cases(baby)
  complete_records <- baby[complete_c,]
  incomplete_records <- baby[!complete_c,]

  #check is the row match
  nrow(complete_records) + nrow(incomplete_records)==nrow(baby)
  #There are no incomplete (missing implicity) values

  #see names
  names(baby)

  #Change names to be more descriptive
  colnames(baby)[7] <- "bwt"
  colnames(baby)[9] <- "mrace"
  colnames(baby)[10] <- "mage"
  colnames(baby)[11] <- "med"
  colnames(baby)[12] <- "mht"
  colnames(baby)[13] <- "mwt"

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
  baby$mrace[c((baby$mrace == 0) | (baby$mrace == 1) | (baby$mrace == 2) | (baby$mrace == 3) | (baby$mrace == 4))] <- 5
  baby$drace[c((baby$drace == 0) | (baby$drace == 1) | (baby$drace == 2) | (baby$drace == 3) | (baby$drace == 4))] <- 5
  baby$med[baby$med == 7] <- 6
  baby$ded[baby$ded == 7] <- 6

  #check data types
  str(baby)

  #Convert variables to factors
  baby$id <- as.factor(as.character(baby$id))
  baby$pluralty <- as.factor(as.character(baby$pluralty))
  baby$outcome <- as.factor(as.character(baby$outcome))
  baby$gestation <- as.numeric(baby$gestation)
  baby$sex <- as.factor(as.character(baby$sex))
  baby$parity <- as.factor(as.character(baby$parity))
  baby$mrace <- as.factor(as.character(baby$mrace))
  baby$med <- as.factor(as.character(baby$med))
  baby$drace <- as.factor(as.character(baby$mrace))
  baby$ded <- as.factor(as.character(baby$ded))
  baby$marital <- as.factor(as.character(baby$marital))
  baby$inc <- as.factor(as.character(baby$inc))
  baby$smoke <- as.factor(as.character(baby$smoke))
  baby$time <- as.factor(as.character(baby$time))
  baby$number <- as.factor(as.character(baby$number))


  #Remove columns that don't show useful information/are all the same
  baby <- baby[-grep('pluralty', colnames(baby))]
  baby <- baby[-grep('outcome', colnames(baby))]
  baby <- baby[-grep('sex', colnames(baby))]
  

  #Convert dates to actual date format
  baby$date <- as.Date((baby$date-1096), origin = "1961-01-01")

  #impute for missing values
  #find na value
  find_na <- apply(baby, 2, is.na)
  impute <- names(which(apply(find_na, 2, any)))
  impute_dat <- baby[c(impute)]

  df_ed_racem<-as.data.frame(cbind(rownames(table(baby$mrace, baby$med)),
                            colnames(table(baby$mrace, baby$med))[apply(table(baby$mrace, baby$med), 1,which.max)]))
  colnames(df_ed_racem)<-c("race", "edu")

  df_ed_raced<-as.data.frame(cbind(rownames(table(baby$drace, baby$ded)),
                                   colnames(table(baby$drace, baby$ded))[apply(table(baby$drace, baby$ded), 1,which.max)]))
  colnames(df_ed_raced)<-c("race", "edu")

  for(z in 1: ncol(impute_dat)){
    u <- impute_dat[ , z ]
    type <- class(u)
    if(type == "numeric" | type == "integer"){
      #impute the mean
      u[which(is.na(u))] = mean(u, na.rm = T)

    }  else{
        #impute the mode
        u[which(is.na(u))] <- sample(as.factor(colnames(t(table(u)[which(table(u)==max(table(u)))]))), 1)
    }

    impute_dat[ , z] <- u
  }

  #check for na values.
  any(is.na(impute_dat))

  #join to the other data
  baby[, names(impute_dat)] <- impute_dat

  #check for na values.
  any(is.na(baby))


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

  return(list(test = test_data, train = schooling))

}
