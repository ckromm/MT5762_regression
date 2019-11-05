#Bootstrap function

#inputs:
#dataset: dataframe in R. The data should allow invertable design mitrices.
#nsim: The number of bootstrapped simulations. The suggested number id 5000 or more,
#      but nsim should be a large number.
#model_fitted: the equation of the model fitted with the lm function. 

library("boot")

spiffy_boots <-function(dataset=data$train, nsim = 5000, model_fitted){
  
  #Bootstrap coeffients function
  get_coefs <- function(dat, idx){
    
    #run the model
    bs_fit <- lm(model_fitted, subset=idx, data=dataset)
    
    #get coeffients
    coef(bs_fit)
  }
  
  #Bootstrap betas
  boot_reg <- boot(dataset, statistic = get_coefs, R = nsim)
  
  #name data
  boot_data <- as.data.frame(boot_reg$t0)
  
  #get confidence intervals
  
  #make empty vectors
  vec_up <- vector()
  vec_down <- vector()
  
  #grab upper and lower confidence values
  for(qc in 1:length(boot_reg$t0)){
    
    dex <- qc
    #upper confidence intervals
    vec_up[qc] <- boot.ci(boot_reg, conf=0.95, type="bca", index = dex)$bca[5]
    #lower confidence intervals
    vec_down[qc] <- boot.ci(boot_reg, conf=0.95, type="bca", index = dex )$bca[4]
    
  }
  #make a dataframe of data  
  ci_df <- as.data.frame(cbind(boot_reg$t0, vec_down, vec_up))
  
  #rename the dataframe
  colnames(ci_df) <- c("Beta Estimate", "Lower Confidence Interval", "Higher Confidence Interval")
  
  return(ci_df)
}

