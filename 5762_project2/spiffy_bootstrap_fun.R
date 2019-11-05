#Bootstrap function

#inputs:
#dataset: dataframe in R. The data be invertable.
#nsim: The number of bootstrapped simulations
#yvar: the response variable of the linear regression of interest. 


spiffy_boots <- function(dataset, nsim, yvar){
  
  #Bootstap function
  bs_fun<-function(df, nsim, response ){
    
    # bootstrap:  resample data with replacement
    boots <- (df[sample(1:nrow(df), nrow(df), replace = T),])
    
    # fit the bootstrapped model 
    regress <- as.formula( paste(response, "~", "."))
    beta <- coef(lm(regress, data= boots))
  }
  
  #run the boot function
  betas_df <- sapply(1:nsim,  function(s) bs_fun(dataset, nsim, yvar))
  boot_df <-as.data.frame(t(betas_df))
  
  #row names
  rownames(boot_df) <- c(1:nsim)
  
  #colnames
  # run the model 
  regress <- as.formula( paste(yvar, "~", "."))
  names_coef <- names(coef(lm(regress, data= dataset)))
  
  colnames(boot_df) <- names_coef
  
  #name c.i.
  boot_ci <-as.data.frame(t(sapply( 1:ncol(boot_df), function(b) quantile(boot_df[ , b], probs = c(0.025, 0.975))))) 
  
  rownames(boot_ci) <- names_coef
  
  return(list(ci= boot_ci, betas = boot_df ))
  
}



fit3data<-cbind(fit3Data, train$wt)
#run of fit 3.
aa<-spiffy_boots(fit3data,100, "wt.1")

