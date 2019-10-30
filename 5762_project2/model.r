source("Finished data cleaning.R")


calculate_MSE <- function(model, test) {
  mse <- mean((test$bwt - predict.lm(model, test)) ^ 2)
  return(mse)
}


logical_model <- function(data) {
  print(head(data))
  print("yeet")

  model <- lm(bwt ~ smoke + time + number + gestation + mage + ded + med + mht + dht + mwt + mrace + inc +
            ded:med + mht:mwt + mrace:med + inc:ded + inc:med + inc:mwt , data=data)

  return(model)
}
data <- get_data("babies23.data")

model <- logical_model(data$train)
mse <- calculate_MSE(model, data$test)
print(mse)
