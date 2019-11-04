source("Finished data cleaning.R")
library("DAAG")
library("caret")
library("car")
library("Hmisc")
library("polycor")
library("lsr")
# -----------------------------------------------------------------------------
calculate_RMSE <- function(model, test) {
  pred <- predict(model, test)
  return(postResample(pred = pred, obs = data$test[,"bwt"]))
}


logical_model <- function(data) {
  train.control <- trainControl(method = "cv", number = 5)

  model <- train(bwt ~ smoke + number + gestation + mage + mht + dht + mwt + inc +
           mht:mwt + inc:mwt , data = data, method = "lm",
       trControl = train.control)

  return(model)
}

data <- get_data("babies23.data")
data$train <- data$train %>% select(-id, -date)
model <- logical_model(data$train)
rmse <- calculate_RMSE(model, data$test)
print(rmse)
# -----------------------------------------------------------------------------
# CORRELATION
library(tidyverse)
library(lsr)

# function to get chi square p value and Cramers V
f = function(x,y) {
    tbl = data$train %>% select(x,y) %>% table()
    chisq_pval = round(chisq.test(tbl)$p.value, 2)
    cramV = round(cramersV(tbl), 2)
    data.frame(x, y, chisq_pval, cramV)
  }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
df_comb = data.frame(t(combn(sort(names(data$train)), 2)), stringsAsFactors = F)

# apply function to each variable combination
df_res = map2_df(df_comb$X1, df_comb$X2, f)

# plot results
df_res %>%
  ggplot(aes(x,y,fill=chisq_pval))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="red", high="yellow")+
  theme_classic()

high_corr <- df_res %>% filter(cramV > 0.7)
high_corr

#Remove (from train and test) columns due to high correlation 
data$train <- data$train[-grep('drace', colnames(data$train))]
data$train <- data$train[-grep('time', colnames(data$train))]
data$test <- data$test[-grep('drace', colnames(data$test))]
data$test <- data$test[-grep('time', colnames(data$test))]
head(data$train)

fullModel <- lm( bwt ~ ., data = data$train)
step(fullModel)
 
# -----------------------------------------------------------------------------

unique(data$train)

aov(data$train[, "bwt"] ~ .,data$train )

corr <- rcorr(as.matrix(data$train), type="pearson")
str(corr)
symnum(corr$n, cutpoints = c(0.5, 0.8), symbols = c("+","*" ))

# Does model analysis
plot(model$finalModel)
train.control <- trainControl(method = "cv", number = 5)

model <- train(bwt ~ gestation + parity + marital + inc + smoke + number + mwt + mht + mrace + mage + med + dwt +
           dht + dage + ded , data = data$train, method = "lm",
     trControl = train.control)


rcorr(data$train)


numericVars <- data$train %>% select_if(is.numeric)
ggpairs(data$train %>% select(-id))

summary(model$finalModel)
vif(model$finalModel)
alias(model$finalModel)

res <- calculate_MSE(model,model$test)
print(res)

attributes(fit)
attributes(model$finalModel)

fit2 = MASS::stepAIC(fit, trace = FALSE)
fit2$anova
summary(fit2)


fit2 = MASS::stepAIC(model$finalModel, trace = FALSE)
fit2$anova
summary(fit2)
any(is.na(train))
train[is.na(train) >0,]

#==============================================================================
# Grace's code
#==============================================================================

install.packages("mice")
install.packages("tableone")
library(tidyverse)
library(lubridate)
library(mice)
library(ggplot2)
library(tableone)
library(boot)
library(caret)

baby = read.table("babies23.data", header = T)

baby = baby %>%
  mutate(pluralty = ifelse(pluralty == 5, "Single Fetus", "Other"),
         date = ymd("1961-01-01") + days(date - 1096),
         sex = recode(as.factor(sex), '1'="male", .default = NA_character_),
         gestation = ifelse(gestation==999, NA, gestation),
         birthwt = ifelse(wt==999, NA, wt),
         parity = ifelse(parity==99, NA, parity),
         race = ifelse(race >= 0 & race <= 5, 0, race),
         race = recode(as.factor(race),
                       '0'="white", '6'="mex", '7'="black",'8'="asian",'9'="mixed", .default = NA_character_),
         age = ifelse(age == 99, NA, age),
         ed = ifelse(ed==6|ed==7, 6, ed),
         ed = recode(as.factor(ed),
                     '0'="less than 8th grade",'1'="8th -12th grade",'2'="HS graduate",'3'="HS + trade",'4'="HS + some colledge",'5'="colledge graduate", '6'='trade school', .default = NA_character_),
         ht = ifelse(ht == 99, NA, ht),
         wt = ifelse(wt.1 == 999, NA, wt.1),
         drace = ifelse(drace >= 0 & drace <= 5, 0, drace),
         drace = recode(as.factor(drace),
                        "0"="white", "6"="mex", "7"="black",'8'="asian",'9'="mixed", .default = NA_character_),
         dage = ifelse(dage == 99, NA, dage),
         ded = ifelse(ded==6|ded==7, 6, ded),
         ded = recode(as.factor(ded),
                      '0'="less than 8th grade",'1'="8th -12th grade",'2'="HS graduate",'3'="HS + trade",'4'="HS + some colledge",'5'="colledge graduate", '6'='trade school', .default = NA_character_),
         dht = ifelse(dht == 99, NA, dht),
         dwt = ifelse(dwt == 999, NA, dwt),
         marital = recode(as.factor(marital),
                          '1'="married",'2'="legally seperated",'3'="divorced",'4'="widowed",'5'="never married",.default = NA_character_),
         inc = recode(as.factor(inc),
                      '0'='under 2500','1'='2500-4999','2'='5000-7499','3'='7500-9999','4'='10000-12499','5'='12500-14999','6'='15000-17499','7'='17500-19999', '8'= '20000-22499','9'='22500+', .default = NA_character_),
         smoke = recode(as.factor(smoke),
                        '0'='never','1'='smokes now','2'='until current pregnancy','3'='once did, not now', .default = NA_character_),
         time = recode(as.factor(time),
                       '0'='never','1'='smokes now','2'='during current preg','3'='within 1 yr','4'='1 to 2 year','5'='2 to 3 year','6'='3 to 4 year', '7'='5 to 9 year','8'='10+ years', .default = NA_character_),
         number = recode(as.factor(number),
                         '0'='never', '1'='1-4','2'='5-9', '3'='10-14', '4'='15-19', '5'='20-29', '6'='30-39', '7'='40-60', '8'='60+', .default = NA_character_)
  ) %>% select(-wt.1)

head(baby)

vars = names(baby)[2:ncol(baby)]
tableone = CreateTableOne(vars = vars, data=baby)
print(tableone, nonnormal = vars, showAllLevels=T, missing=T, minMax = T)

baby = baby %>%
  mutate(
    inc = as.numeric(inc),
    number = recode(as.numeric(number),
                    `1`=0L, `2`=2L,`3`=7L, `4`=12L, `5`=17L, `6`=24L, `7`=34L, `8`=50L, `9`=60L, .default = NA_integer_),
    month = as.factor(month(date)) # birth month
  ) %>%
  select(-pluralty, -outcome, -sex, -date, -time)

head(baby)
any(is.na(baby))

idx = sample(c(1:nrow(baby)), floor(nrow(baby)*0.8), replace = F)
train = baby[idx,]
validation = baby[-idx,]

qqnorm(train$birthwt); qqline(train$birthwt, col = 2)
c("mean"=mean(train$birthwt), 'median'=median(train$birthwt))

fit = lm(birthwt ~ gestation + parity + marital * inc + smoke * number + month + wt + ht + race + age + ed + dwt +
           dht + drace + dage + ded, train)
summary(fit)

fit2 = MASS::stepAIC(fit, trace = FALSE)
fit2$anova
summary(fit2)

model <- train(birthwt ~ gestation + parity + smoke + number + wt + ht + dwt + drace, data = train, method = "lm",
     trControl = train.control, na.action=na.omit)

res <- calculate_MSE(model,validation)
print(res)

fit3 = lm(birthwt ~ gestation + parity + smoke + number + wt + ht + dwt + drace, train)
summary(fit3)
anova(fit3)

#==============================================================================
# Model diagnostics
#==============================================================================

install.packages("GGally")
library(GGally)
library(car)
library(lmtest)
library(sandwich)

# setwd("5762_project2");  // I need to run that
# Normality
qqnorm(resid(fit3))
qqline(resid(fit3))
shapiro.test(resid(fit3))
# passes the test we can assume normality

# Constant spread
fitResid <- resid(fit3)
plot(fitted(fit3), fitResid, ylab = "Residuals", xlab = "Fitted values")
# The residuals "bounce randomly" around the 0 line - the assumption that the relationship is linear is reasonable.
# The residuals roughly form a "horizontal band" around the 0 line - the variances of the error terms are ~equal.
# Though might have an issue since concentrated in the middle.
# No one residual "stands out"  a lot from the basic random pattern of residuals - no  big outliers.
ncvTest(fit3)
# p-value of 0.19338 - we're okay here though
bptest(fit3)
# p-value of 0.0001097 - indicates presence of heteroskedasticity (studentize the original BP test)
# compare to White standard errors to see if they're inflated
summary(fit3)
coeftest(fit3, vcov = vcovHC(fit3, "HC1"))
# standard errors are pretty similar to robust White se so we are okay

# Standard plots
par(mfrow=c(2,2))
plot(fit3)

head(train)

# Collinearity
fit3Data <- train[,-c(1,2,6,7,8,11,12,13,15,16,19)]
head(fit3Data)

numericVars <- fit3Data %>% select_if(is.numeric)
ggpairs(numericVars)
# Correlation doesn't seem to be an issue but check VIF
vif(fit3)
# Really small VIF values - none of them are even close to 10 => ok
