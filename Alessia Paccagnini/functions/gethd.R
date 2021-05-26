#=======================================================================================================
#                               gethd.R
#=======================================================================================================
# This function computes computes Historical Decompositions (HD)
#
# Notes. This function is based on Ambrogio Cesa-Bianchi's matlab code - available on  his personal 
# webpage.
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


gethd <- function(datatemp,Betahd,invAhd,RESIDS,const,lagENDO) {


# Information
kv <- ncol(Betahd)
capT <- nrow(datatemp)
TESS <- capT - lagENDO

# Get contemporaneous and lagged variables
ytemp <- datatemp[(1+lagENDO):capT, ]  
xtemp <- getlag(datatemp,lagENDO,const) # Get lagged endogenous variables 

# Get companion
THETA <- getcomp(Betahd,lagENDO) # reduced form OLS coefficients
compinvA0 <- rbind(invAhd, matrix(0,nrow = (kv *(lagENDO-1)), ncol = kv)) # structural impact multiplier matrix

# Selection matrix (useful when p > 1)
selID <- cbind(diag(kv), matrix(0, nrow = kv, ncol = (lagENDO-1)*kv))

# Getting structural shocks 
eStruct <- solve(invAhd) %*% t(RESIDS)

#------------------------------------------------
# Stochastic component
compHD.stoch <- array(0, dim=c(kv*lagENDO,TESS+1, kv))
HD.stoch <- array(0, dim=c(kv,TESS+1, kv))

for (kk in 1 : kv){  # for each shock
  
  EPS <- matrix(0, nrow = kv, ncol = TESS+1) 
  EPS[kk , 2:(TESS+1)] <- eStruct[kk,]
  
  for (tt in 2 : (TESS+1)) {
    
    compHD.stoch[ , tt , kk] <- THETA %*% compHD.stoch[ , tt-1, kk] + compinvA0 %*% EPS[ , tt]
    HD.stoch[, tt , kk] <- selID %*% compHD.stoch[ , tt , kk]
    
  }

} 

#------------------------------------------------
# Initial condition
compHDinit <- matrix(0, nrow = kv*lagENDO, ncol = TESS+1)
compHDinit[,1] <- matrix(xtemp[1,-ncol(xtemp)])
HDinit <- matrix(0, nrow = kv, ncol = TESS+1)
HDinit[,1] <- selID %*% compHDinit[,1]

for (tt in 2 : (TESS+1)) {
  
  compHDinit[,tt] <- THETA %*% compHDinit[,tt-1]
  HDinit[,tt] <- selID %*% compHDinit[,tt]
  
}

#------------------------------------------------
# Constant term
if (const == 1) {
  
compHDconst <- matrix(0, nrow = kv*lagENDO, ncol = TESS+1)  
HDconst <- matrix(0, nrow = kv, ncol = TESS+1)  

compMU <- rbind(matrix(Betahd[(kv*lagENDO)+1 , ]) , matrix(0, nrow = kv*(lagENDO-1), ncol = 1))

  for (tt in 2 : (TESS+1)) {  
    
    compHDconst[,tt] <- compMU + THETA %*% compHDconst[,tt-1] 
    HDconst[,tt] <- selID %*% compHDconst[,tt]
    
  }

} else {

  HDconst <- matrix(0, nrow = kv, ncol = TESS+1)  

}

# Check if one retrives the original series. 
# Notes. The VAR is specified as a VAR(1) using the companion representation
HDdata <- HDconst + HDinit + rowSums(HD.stoch, dims = 2)

if (all(round(t(HDdata), 9) == round(datatemp[lagENDO : nrow(datatemp) , ] , 9)) == TRUE) {
  
  print('The original series have been retrieved')
  
  PROBLEM <- 0
  
} else {
  
  PROBLEM <- 1
  
}

# Print historical decomposition of the structural shocks [T x SHOCK x VARIABLE)
HDshock <- array(0, dim=c(TESS+1, kv, kv), dimnames = list(NULL, paste0('eps',1:kv), paste0('y',1:kv)))

for (ii in 1 : kv) { # for each variable
  
  for (jj in 1 : kv) { # for each shock
    
    HDshock[,jj,ii] <- matrix(HD.stoch[ii,,jj])
    
  }

}    

HDdecomp <- list(HDunexp=HDshock,HDconst=HDconst,HDinit=HDinit,INITCOND=t(HDconst+HDinit),PROBLEM=PROBLEM)


return(HDdecomp)

}


