

#load data
baby <- read.table("../../5762_project2/5762_project2/babies23.data")
metadata <- read.delim("../../5762_project2/5762_project2/babies.readme.txt")

#remae column
colnames(metadata)<-c("label")

#get names from titles
metalab1 <- sapply(1:nrow(metadata), function(ox) unlist(strsplit(as.character(metadata[ox, 1]),  "-")[1])[1])
metalab2 <- sapply(1:nrow(metadata), function(p) unlist(strsplit(metalab1[p], " ")), simplify = "array")

#intialise loop
labels <-vector()
i <-0
for (td in 1:nrow(metadata)) {
    #get only the titles for the variables.
    labels1 <- metalab2[[td]]
    labels2 <-list(strsplit(labels1, " "))
    if(length(unlist(labels2)) == 2){
      i <- i + 1
      labels[i] <- list(strsplit(unlist(labels2), " ")[2])
    }
  
}

#add the "correct" names.
colnames(baby) <- unlist(labels)

#change names!



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


