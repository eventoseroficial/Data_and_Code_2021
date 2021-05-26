#=======================================================================================================
#                  getlag.R
#=======================================================================================================
# This function constructs the matrix containing the lagged endogenous variables plus the intercept 
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


getlag <- function(DATAMAT,plag,CONSTANT) {

if (is.matrix(DATAMAT) == FALSE) {
  
  DATAMAT <- matrix(DATAMAT)

}

kvar <- ncol(DATAMAT)
TOBS <- nrow(DATAMAT)
TESS <- TOBS - plag

X <- matrix(0, nrow = TESS, ncol = 0)

# Lagged endogenous variables
for (tt in 1 : plag) {

  X <- cbind(X , DATAMAT[((1+plag)-tt) : (TOBS-tt) , ])  

}

if (!is.null(colnames(DATAMAT))) {
  
  colnames(X) <- paste0(rep(colnames(DATAMAT) , plag), '_l', rep(1:plag, each=kvar))

} else {
  
  colnames(X) <- paste0(rep(paste0('y', 1:kvar), plag), '_l', rep(1:plag, each=kvar))

}  

# Constant
if (CONSTANT == 1) {
  
  X <- cbind(X , rep(1 , TESS))   
  colnames(X)[ncol(X)] <- 'constant'

} else if(CONSTANT == 0) {
  
  X <- X

}

return(X)

}


