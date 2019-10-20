require(tidyverse)

#load dataDocuments\5762_project2\5762_project2\Mt5762_regression
baby <- read.table("C:\\Users\\Christopher\\Documents\\5762_project2\\5762_project2\\Mt5762_regression\\5762_project2\\babies23.data")
metadata <- read.delim("C:\\Users\\Christopher\\Documents\\5762_project2\\5762_project2\\Mt5762_regression\\5762_project2\\babies.readme.txt")

#remae column
colnames(metadata)<-c("label")

#grab rows of interest.
metalab1 <- sapply(1:nrow(metadata), function(ox) unlist(strsplit(as.character(metadata[ox, 1]),  "-" )[1])[1])
metalab1_index <- sapply(1:nrow(metadata), function(ox) unlist(strsplit(as.character(metadata[ox, 1]),  " " )[1])[1])

#get only the 23 names (ignore error message)
metalab1_names <- metalab1[which(!is.na(as.numeric(metalab1_index)))]
metalab2_names <- sapply(1:length(metalab1_names), function(p) unlist(strsplit(metalab1_names[p], " "))[2], simplify = "array")


#add the "correct" names.
colnames(baby) <- metalab2_names


#change names!
colnames(baby)[7] <- "Chien"


#the names are wrong!
names(baby)

#check for na values
which(is.na(baby))
#There are no na value

#check for missing values
complete_c <- complete.cases(baby)
complete_records <- baby[complete_c,]
incomplete_records <- baby[!complete_c,]

#check is the row match
nrow(complete_records) + nrow(incomplete_records)==nrow(baby)
#There are no incomplete (missing implicity) values

#split data by id (because babies are the sampling unit).

#TEST SET
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

