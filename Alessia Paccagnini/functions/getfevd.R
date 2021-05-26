#=======================================================================================================
#                           getfevd.R 
#=======================================================================================================
# This function computes the Forecast Error Variance Decomposition (FEVD) 
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


getfevd <- function(Bmat, A0mat, L, fhorizon) {

  
if (is.null(A0mat)) {
  stop('No structural dynamic analysis can be performed! Specify impact multiplier matrix')
}
  
# Number of endogenous variables
nvar <- ncol(Bmat)    

# Names of endogenous variables
if (is.null(colnames(Bmat)) == FALSE) {
  
  labVAR <- colnames(Bmat)
  
} else {
  
  labVAR <- paste0('y', 1:nvar)
  
}  

# 1. Compute IRFs  
# Get companion
MATCOMP <- getcomp(Bmat,L) 

# Selection matrices
J <- rbind( diag(nvar) , matrix(0, nrow = (nvar*L)-nvar, ncol = nvar))  

# Save IRFs 
THETA <- array(0, dim = c(nvar, nvar, fhorizon+1), dimnames = list(NULL , paste0('eps', 1:nvar), 
                                                                   paste0('horz',0:fhorizon)))

for (hh in 1 : (fhorizon+1)) {
    
  THETA[,,hh] <- t(J) %*% (MATCOMP%^%(hh-1)) %*% J %*% A0mat
    
}  

SIGMAEPS <- diag(nvar)
  
# 2. Prediction Mean Squared Error 
MSPE <- array(0, dim = c(nvar , nvar , fhorizon), dimnames = list(labVAR, paste0('eps', 1:nvar) , paste0('hor', 1:fhorizon)))

MSPE[,,1] <- THETA[,,1]^2 # Initialize MSPE

for (hh in 2 : fhorizon) {
  
  MSPE[,,hh] <- ( THETA[,,hh]^2 ) + MSPE[,,hh-1]
  
}


# 3. Total Prediction Mean Squared Error 
TOT.MSPE <- array(0, dim = c(nvar , nvar , fhorizon), dimnames = list(labVAR, paste0('eps', 1:nvar) , paste0('hor', 1:fhorizon)))

TOT.MSPE[,,1] <- THETA[,,1] %*% SIGMAEPS %*% t(THETA[,,1]) # Initialize MSPE

for (hh in 2 : fhorizon) {
  
  TOT.MSPE[,,hh] <- ( THETA[,,hh] %*% SIGMAEPS %*% t(THETA[,,hh]) ) + TOT.MSPE[,,hh-1]
  
}

# 4. Forecast Error Variance Decomposition
fevd <- array(0, dim = c(nvar , nvar , fhorizon), dimnames = list(labVAR, paste0('eps', 1:nvar) , paste0('hor', 1:fhorizon)))

for (hh in 1 : fhorizon) {
  
  for(ii in 1 : nvar) {
    
    for (jj in 1 : nvar) {
      
      fevd[ii,jj,hh] <- MSPE[ii,jj,hh] / TOT.MSPE[ii,ii,hh]
      
    }
    
  }
  
}

# For each forecast horizon the row sum must sum up to one 
check <-apply(fevd, MARGIN = 3, function(x) rowSums(x))

if (round(sum(apply(check, MARGIN = 1, function(x) sum(x)))) != nvar*fhorizon) {
  
  message('Error! Row sum condition is not satisfied!')
  
}

# Reshape FEVD [HORIZON x VARIABLE x SHOCK]
PFEVD <- printfevd(fevd)

return(PFEVD)


}
