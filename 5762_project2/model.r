source("Finished data cleaning.R")
library("DAAG")

calculate_MSE <- function(model, test) {
  mse <- mean((test$bwt - predict.lm(model, test)) ^ 2)
  return(mse)
}

perform_cv <- function(model, data) {
  cross_v<-cv.lm(data=data, model, m=3)
}


logical_model <- function(data) {
  print(head(data))

  model <- lm(bwt ~ smoke + number + time + gestation + mage + ded + med + mht + dht + mwt + mrace + inc +
            ded:med + mht:mwt + mrace:med + inc:ded + inc:med + inc:mwt , data=data)

  return(model)
}
data <- get_data("babies23.data")

model <- logical_model(data$train)

summary(model)
mse <- calculate_MSE(model, data$test)
print(mse)
perform_cv(model, data$train)

t <-  data$train
str(t[,"number"])
class(t[,"number"])
