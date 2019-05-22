#this is a script from Brandon Chasco about simulating data


#1) Parameters - these are affectively your hypotheses
#2) The rule that determines if your hypothesis is a "good" one
#3) The data - this can be simulated or real data


set.seed(2) #The seed for the random number generator
myData <- rnorm(100,mean=10,sd=1)

#This is the rule I am using to find the parameter that
#best explains my data
myRuleForBestEstimate <- function(par,data){
  #outputFromMyRule <- sum((data-par)^2)
  
  #Replace the first rule with a statistical rule (i.e., a probability distributions/likelihood)
  outputFromMyRule <- -1*sum(log(dnorm(data,mean=par[1],sd=exp(par[2]))))
  
  #This is a single value summarizing the
  #difference between my parameter estimate and the data
  print(outputFromMyRule)
  return(outputFromMyRule)
}

#Capture the output from the optimization
opt <- optim(c(0,0),
             myRuleForBestEstimate,
             data=myData)


#Print the parameter estimate
print(opt$par, silent=TRUE)

simulateVB <- function(Linf,K,t0,sampleSize,sampleErr){
  #estimated Lengths
  lens <- NA
  #Sample the ages
  age <- sample(1:20,sampleSize,replace=TRUE)
  #
  for(i in 1:sampleSize){
    lens[i] <- Linf*(1-exp(-K*(age[i]-t0)))  *  exp(rnorm(1,0,sampleErr))
  }
  
  return(list(age=age, #simulated ages
              lens=lens)) #simulated lengths
  
}

set.seed(1)
simData <- simulateVB(100,0.1,-1,100,0.1)

#best explains my data
myRuleForVB <- function(par,data){
  #outputFromMyRule <- sum((data-par)^2)
  #Take the parameter vector from optim and turn it into parameters that are meaningful for the VB model
  Linf <- par[1]
  k <- exp(par[2])
  t0 <- par[3]
  cv <- exp(par[4])
  
  #Predictions
  VBest <- Linf * (1 -exp(-k*(data$age - t0)))
  
  #negative log of the log-normal likelihood. Logging the observed and predicted values makes it log-normal
  outputFromMyRule <- -1*sum(log(dnorm(log(data$lens),mean=log(VBest),sd=cv)))
  
  #This is a single value summarizing the
  #difference between my parameter estimate and the data
  print(outputFromMyRule)
  return(outputFromMyRule)
}

#Run optim and capture the output
opt <- optim(c(90,log(0.1),-1,log(10)),
             myRuleForVB,
             data=simData)

#print theparameter estimates, remembering to do the necessary transformations
print(paste(opt$par[1], exp(opt$par[2]), opt$par[3], exp(opt$par[4])))


#Plot simulated data
plot(simData$age,simData$lens)

#Model prediction
lines(0:20,
      opt$par[1]*(1-exp(-exp(opt$par[2])*(0:20-opt$par[3]))),
      col="red",
      lwd=3)
