source("Finished data cleaning.R")
library("DAAG")
library("caret")

calculate_MSE <- function(model, test) {
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

model <- logical_model(data$train)
mse <- calculate_MSE(model, data$test)
print(mse)

# Does model analysis
plot(model$finalModel)
