source("Finished data cleaning.R")
library("DAAG")
library("caret")
library("car")
library("Hmisc")
library("polycor")
library("lsr")
library("tidyverse")
library("MASS")
library("GGally")
library("car")
library("lmtest")
library("sandwich")

# rmse and r^2 function
calculate_RMSE <- function(model, test_data) {
  pred <- predict(model, test_data)
  return(postResample(pred = pred, obs = test_data[,"bwt"]))
}

# load cleaned data
data <- get_data("babies23.data")
# remove id and date, as they were not relevant
data$train <- data$train %>% dplyr::select(-id, -date)
# ----------------------------------------------------------------------
# CORRELATION
# adapted from https://stackoverflow.com/a/52557631

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

# filter correlation for greater than 0.7
high_corr <- df_res %>% filter(cramV > 0.7)
high_corr
# -----------------------------------------------------------------------
# Model

# Remove (from train and test) columns due to high correlation
data$train <- data$train[-grep('drace', colnames(data$train))]
data$train <- data$train[-grep('time', colnames(data$train))]
data$test <- data$test[-grep('drace', colnames(data$test))]
data$test <- data$test[-grep('time', colnames(data$test))]
head(data$train)

# Perform Cross Validation
train.control <- trainControl(method = "cv", number = 5)

# Run the first model, a priori selection
model_1 <- train(bwt ~ smoke + number + gestation + mage + mht + dht + mwt + inc +
         mht:mwt + inc:mwt , data = data$train, method = "lm",
     trControl = train.control)
summary(model_1)
rmse_1 <- calculate_RMSE(model, data$test)
mse_1 <- rmse_1[1] ^ 2
r2_1 <- rmse_1[2]
mse_1
r2_1

# Run the full model stepped
model_2_full <- train(bwt ~ . , data = data$train, method = "lm", trControl = train.control)
model_2 <- step(lm(model_2_full, data=data$train))
summary(model_2)
rmse_2 <- calculate_RMSE(model_2, data$test)
mse_2 <- rmse_2[1] ^ 2
r2_2 <- rmse_2[2]
mse_2
r2_2

# Run model 3 stepwise based on BIC
fullModel <- lm( data$train$bwt ~ ., data = data$train)
nullModel <- lm(bwt~ 1, data = data$train)
model_3_step <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), data = data$train, direction='forward',
                   k=log(nrow(data$train)))
model_3 <- train(as.formula(model_3_step), data=data$train, method = "lm", trControl = train.control)
summary(model_3)
rmse_3 <- calculate_RMSE(model_3, data$test)
mse_3 <- rmse_3[1] ^ 2
r2_3 <- rmse_3[2]
mse_3
r2_3

# code to check to make sure that model3 isn't using aic and is doing it based on BIC
model_4 <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), data = data$train, direction='forward',
                k=2)
summary(model_4)


source("working_bootstrap.R")

# Run the bootstrap on the best model (model_2)
bootstrap <- spiffy_boots(data$train, 2000, model_2)
round(bootstrap, 2)
#==============================================================================
# Model diagnostics
#==============================================================================

# Model - model_2
#------------------------------------------------------------------------------
diagnostics <-  function(model_2){
# setwd("5762_project2");  // I need to run that
# Normality
qqnorm(resid(model_2))
qqline(resid(model_2))
shapiro.test(resid(model_2))
# doesn't pass the test (which happens for a big dataset) but data looks pretty normal can assume normality

# Constant spread
fitResid <- resid(model_2)
plot(fitted(model_2), fitResid, ylab = "Residuals", xlab = "Fitted values")
# The residuals "bounce randomly" around the 0 line - the assumption that the relationship is linear is reasonable.
# The residuals roughly form a "horizontal band" around the 0 line - the variances of the error terms are ~equal.
# Though might have an issue since concentrated in the middle.
# No one residual "stands out"  a lot from the basic random pattern of residuals - no  big outliers.
ncvTest(model_2)
# p-value of 0.19338 - we're okay here though
bptest(model_2)
# p-value of 0.0001097 - indicates presence of heteroskedasticity (studentize the original BP test)
# compare to White standard errors to see if they're inflated
summary(model_2)
coeftest(model_2, vcov = vcovHC(model_2, "HC1"))
# standard errors are pretty similar to robust White se so we are okay

# Standard plots
par(mfrow=c(2,2))
plot(model_2)

head(data$train)

# Collinearity
model_2Data <- data$train[,-c(5,6,9,10,11,13,14)]
head()

numericVars <- model_2Data %>% select_if(is.numeric)
ggpairs(numericVars)
# Correlation doesn't seem to be an issue but check VIF since we only do numeric vars above and ignore factors
vif(model_2)
}
# Really small VIF values - none of them are even close to 10 => ok

# Model - model_3
#------------------------------------------------------------------------------
# diagnostics(model_3)
# Normality
par(mfrow=c(1,1))
qqnorm(resid(model_3))
qqline(resid(model_3))
shapiro.test(resid(model_3))
# doesn't pass the test (which happens for a big dataset) but data looks pretty normal can assume normality

# Constant spread
fitResid <- resid(model_3)
plot(fitted(model_3), fitResid, ylab = "Residuals", xlab = "Fitted values")
# The residuals "bounce randomly" around the 0 line - the assumption that the relationship is linear is reasonable.
# The residuals roughly form a "horizontal band" around the 0 line - the variances of the error terms are ~equal.
# Though might have an issue since concentrated in the middle.
# No one residual "stands out"  a lot from the basic random pattern of residuals - no  big outliers.
ncvTest(model_3)
# p-value of 0.22043 - we're okay here though
bptest(model_3)
# p-value of 0.2346 - indicates no presence of heteroskedasticity (studentize the original BP test)

# Standard plots
par(mfrow=c(2,2))
plot(model_3)

head(data$train)

# Collinearity
model_3Data <- data$train[,-c(3,4,5,6,8,9,10,11,13,14,16)]
head(model_3Data)

numericVars <- model_3Data %>% select_if(is.numeric)
ggpairs(numericVars)
# Correlation doesn't seem to be an issue but check VIF since we only do numeric vars above and ignore factors
vif(model_3)
# Really small VIF values - none of them are even close to 10 => ok
