source("Finished data cleaning.R")
library("DAAG")
library("caret")

calculate_MSE <- function(model, test) {
  mse <- mean((test$bwt - predict.lm(model, test)) ^ 2)
  return(mse)
}

# perform_cv <- function(model, data) {
#   cross_v<-cv.lm(data=data, model, m=3)
# }


logical_model <- function(data) {
  train.control <- trainControl(method = "cv", number = 5)
  # model <- lm(bwt ~ smoke + number + time + gestation + mage + ded + med + mht + dht + mwt + mrace + inc +
  #           ded:med + mht:mwt + mrace:med + inc:ded + inc:med + inc:mwt , data=data)
  # model <- lm(, data=data)

  model <- train(bwt ~ smoke + number + gestation + mage + mht + dht + mwt + inc +
           mht:mwt + inc:mwt , data = data, method = "lm",
       trControl = train.control)

  return(model)
}
data <- get_data("babies23.data")
# new_train <- data$train %>% select(-mrace, -drace)
# model <- logical_model(new_train)
# summary(model)
# summary(data$test)
# mse <- calculate_MSE(model, data$test %>% select(-drace))
# print(mse)
# perform_cv(model, new_train)

levels(data$test[,"number"]) <- levels(data$train[,"number"])
lapply(data$train, unique)
lapply(data$test, unique)
# unique(data$train[,"number"])
#
# model <- logical_model(data$train)
# summary(model)
# mse <- calculate_MSE(model, data$test)
# print(mse)
# perform_cv(model, data$train)
#
#
# data$test %>% dplyr::mutate_all(as.factor) %>% str

source("cleantest.R")
data <- get_data("babies23.data")
t <- data$train
head(t)
hist(as.numeric(t$mrace))

model <- logical_model(data$train)
summary(model)
mse <- calculate_MSE(model, data$test)
print(mse)
perform_cv(model, data$train)

# Does model analysis
plot(model)
