#Bootstrap function

#inputs:
#dataset: dataframe in R.
#nsim: The number of bootstrapped simulations
#yvar: the response variable of the linear regression of interest. 

spiffy_boots <- function(dataset, nsim, yvar){
  
  #Bootstap function
  bs_fun<-function(inputData, nsim, Yvariable ){

    # bootstrap:  resample data with replacement
    boots <- (inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),])
    
    # fit the bootstrapped model 
    regress <- as.formula( paste(Yvariable, "~", "."))
    beta <- coef(lm(regress, data= boots))
  }
  
  #boot_beta <- parSapply(cluster, 1:nsim, par_fun, bData = Data, Yvariable= Yvar)
  betas_df <- sapply(1:nsim,  function(s) bs_fun(dataset, nsim, yvar))
  boot_df <-as.data.frame(t(betas_df))
  
  #row names
  rownames(boot_df) <- c(1:nsim)
  
  #colnames
  regress <- as.formula( paste(yvar, "~", "."))
  names_coef <- names(coef(lm( regress, data= dataset)))
  
  colnames(boot_df) <- names_coef
  
  #get c.i.
  boot_ci <-as.data.frame(t(sapply( 1:ncol(boot_df), function(b) quantile(boot_df[ , b], probs = c(0.025, 0.975))))) 
  
  
  #name c.i.
  rownames(boot_ci) <- names_coef
  
  return(list(ci= boot_ci, betas = boot_df ))
  
}


