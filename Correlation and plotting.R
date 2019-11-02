source("5762_project2/Finished data cleaning.R")

library(ggplot2)
library(tidyverse)
library(arm)
library(dplyr)
library(ggpubr)
library(GGally)

data <- get_data("5762_project2/babies23.data")
training <- data$train



#Delete NA value
trainnoNA <- na.omit(training)


#Calculate coefficient for numeric variables
numeric <- trainnoNA %>% dplyr::select(bwt, mage, mht, mwt, dage, dht, dwt)
cor(numeric)
ggcorr(numeric, label = TRUE, label_alpha = TRUE)


#ANOVA test for correlation between categorical and numeric variables
incaov <- aov(bwt ~ inc, data = trainnoNA)
summary(inc)

mraceaov <- aov(bwt ~ mrace, data = trainnoNA)
summary(mraceaov)

medaov <- aov(bwt ~ med, data = trainnoNA)
summary(medaov)

dedaov <- aov(bwt ~ ded, data = trainnoNA)
summary(dedaov)

smokeaov <- aov(bwt ~ smoke, data = trainnoNA)
summary(smokeaov)

timeaov <- aov(bwt ~ time, data = trainnoNA)
summary(timeaov)

numberaov <- aov(bwt ~ number, data = trainnoNA)
summary(numberaov)
dage <- as.numeric(trainnoNA$dage)


#Scatter plots with mean and sd
magem <- ggerrorplot(trainnoNA, x = "mage", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray") + labs(x = "Mother's age", y = "Baby's birth weight"))
magem

dagem <- ggerrorplot(trainnoNA, x = "dage", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray") + labs(x = "Dad's age", y = "Baby's birth weight"))
dagem

mhtm <- ggerrorplot(trainnoNA, x = "mht", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Mother's height", y = "Baby's birth weight")
mhtm

mwtm <- ggerrorplot(trainnoNA, x = "mwt", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Mother's weight", y = "Baby's birth weight")
mwtm

dwtm <- ggerrorplot(trainnoNA, x = "dwt", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Dad's weight", y = "Baby's birth weight")
dwtm

dhtm <- ggerrorplot(trainnoNA, x = "dht", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Dad's height", y = "Baby's birth weight")
dhtm

incm <- ggerrorplot(trainnoNA, x = "inc", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Income \n (0 = under 2500,
            1=2500-4999, ..., 8= 12,500-14,999, 9=15000+)", y = "Baby's birth weight")
incm

mracem <- ggerrorplot(trainnoNA, x = "mrace", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Mother's race \n (5=white 6=mex 7=black 8=asian 9=mixed)", y = "Baby's birth weight")
mracem

dracem <- ggerrorplot(trainnoNA, x = "drace", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Dad's race \n (5=white 6=mex 7=black 8=asian 9=mixed)", y = "Baby's birth weight")
dracem

medm <- ggerrorplot(trainnoNA, x = "med", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Mother's education \n (0= less than 8th grade, 
            1 = 8th -12th grade - did not graduate, 
            2= HS graduate--no other schooling , 3= HS+trade,
            4=HS+some college 5= College graduate)", y = "Baby's birth weight")
medm

dedm <- ggerrorplot(trainnoNA, x = "ded", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Dad's education \n (0= less than 8th grade, 
            1 = 8th -12th grade - did not graduate, 
            2= HS graduate--no other schooling , 3= HS+trade,
            4=HS+some college 5= College graduate, 6 Trade school HS unclear)", y = "Baby's birth weight")
dedm

smoke <- ggerrorplot(trainnoNA, x = "smoke", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Mother smokes or not \n (0=never, 1= smokes now, 
            2=until current pregnancy, 3=once did, not now)", y = "Baby's birth weight")
smoke

time <- ggerrorplot(trainnoNA, x = "time", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "How long for mother to quit smoking \n (0=never smoked, 1=still smokes,
            2=during current preg, 3=within 1 yr, 4= 1 to 2 years ago,
            5= 2 to 3 yr ago, 6= 3 to 4 yrs ago, 7=5 to 9yrs ago, 
            8=10+yrs ago, 9=quit and don't know)", y = "Baby's birth weight")
time

number <- ggerrorplot(trainnoNA, x = "number", y = "bwt", 
            desc_stat = "mean_sd", color = "darkblue",
            add = "jitter", add.params = list(color = "darkgray")) + labs(x = "Number of cigs smoked per day for past and current smokers \n  (0=never, 1=1-4
            2=5-9, 3=10-14, 4=15-19, 5=20-29, 6=30-39, 7=40-60)", y = "Baby's birth weight")
number
