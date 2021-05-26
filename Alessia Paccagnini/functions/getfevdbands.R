#=======================================================================================================
#                              getfevdbands.R 
#=======================================================================================================
# This function computes confidence intervals for Forecast Error Variance Decomposition through 
# residual-based bootstrap
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


getfevdbands <- function(datamat,bigL,CONSTANT,BETA,capinvA0,FHORZ,cb,ndraws) {


# Get point estimates
fevdpe <- getfevd(BETA,capinvA0, bigL, FHORZ) 

# Get confidence intervals
# Data  
tobs <- nrow(datamat)
temp <- tobs - bigL
kvar <- ncol(datamat)

# Get data matrices for estimation
YDATA <- datamat[(1+bigL):tobs , ]
XDATA <- getlag(datamat,bigL,CONSTANT)

# Get OLS residuals (to be used in the simulation - that is residuals-based bootstrap) 
resids <- YDATA - XDATA%*%BETA

# Constant
if (CONSTANT == 1) {
  
  const <- matrix(rep(1 , temp))   
  
} else {
  
  const <- NULL
  
}

deterministic <- const


# Get confidence bands
# Bootstrapping the residuals
ucent <- apply(resids, MARGIN = 2, function(x) scale(x, center = TRUE, scale = FALSE))

# Save impulse response functions
FEVDBOOT <- vector('list', ndraws)

#------------------------------------------------------------------------------------------
# Bootstrap starts here
irep <- 1 # Initialize

while (irep <= ndraws) {
  
  nb <- sample(1:temp, replace = TRUE) # extract from uniform distribution with replacement
  uboot <- ucent[nb , ] # bootstrap the residuals
  ubt <- rbind(matrix(0, nrow = bigL, ncol = kvar) , uboot)
  
  # Initialize data boot matrix
  databoot <- matrix(0, nrow = tobs, ncol = kvar)
  databoot[1:bigL , ] <- datamat[1:bigL , ]   
  
  # Reconstruct the series
  for (tt in (1+bigL) : tobs) {
    
    xb <- matrix(t(databoot[ rev((tt-p):(tt-1))   ,  ]) , nrow = 1, byrow = TRUE)
    xboot <- c(xb, deterministic[tt-bigL , ])
    databoot[tt , ] <- t(xboot) %*% BETA + ubt[tt , ]
    remove(xb); remove(xboot);
    
  }
  
  # Get data matrices for estimation
  Yboot <- databoot[(1+bigL):tobs , ]
  Xboot <- getlag(databoot,bigL,CONSTANT)
  N <- ncol(Xboot) # Number of coefficients for each VAR equation
  
  # Get OLS quantities 
  Bboot <- solve(t(Xboot)%*%Xboot) %*% t(Xboot)%*%Yboot 
  residboot <- Yboot - Xboot%*%Bboot
  sseboot <- crossprod(residboot)
  SIGMAboot <- (1 / (temp-N)) * sseboot
  
  
  # Identification scheme
  invA0b <- t(chol(SIGMAboot))
  
  # Save only non-explosive FEVD
  THETA <- getcomp(Bboot,bigL) 
  
  eigboot <- eigen(THETA)$values
  
  if (max(abs(eigboot)) < 1) {
    
    FEVDBOOT[[irep]] <- getfevd(Bboot,invA0b,bigL,FHORZ)
    
    print(paste0('Bootstrap replication: ',irep))
    
    irep <- irep + 1
    
  }   
  
}

# Bootstrap ends here
#------------------------------------------------------------------------------------------

# Construct confidence intervals 
# Create matrix for the distribution of the parameters
distrpar <- matrix(0, nrow = kvar^2*FHORZ , ncol = ndraws)
colnames(distrpar) <- paste0('iter',1:ndraws)
npars <- nrow(distrpar)

for (ii in 1 : ndraws) {
  
  distrpar[,ii] <- matrix(FEVDBOOT[[ii]])
  
}  

# Compute lower and upper bands
siglev <- 1 - cb
lowb <- siglev / 2
uppb <- 1 - (siglev / 2)

qnts <- t(apply(distrpar, MARGIN = 1, function(x) quantile(sort(x) , c(lowb , 0.5 , uppb) )))

LWbands <- array(qnts[,1] , dim = c(FHORZ, kvar, kvar))
MEDbands <- array(qnts[,2] , dim = c(FHORZ, kvar, kvar))
UPbands <- array(qnts[,3] , dim = c(FHORZ, kvar, kvar))

# Create output for FEVD [HORIZON x PERCENTILES x VARIABLE x SHOCK]
FEVDOUTPUT <- array(0, dim = c(FHORZ, 3, kvar, kvar) , dimnames = list(paste0('H=', 1:FHORZ), c('LW','MEDIAN','UB'), paste0('y', 1:kvar), paste0('eps', 1:kvar)))

for (ishock in 1 : kvar) { # for each shock
  
  for (kk in 1 : kvar)  {
    
    FEVDOUTPUT[,,kk,ishock]  <- cbind(LWbands[,kk,ishock],MEDbands[,kk,ishock],UPbands[,kk,ishock]) # k-th variable, i-th shock
    
  }
  
}

return(FEVDOUTPUT)

}
