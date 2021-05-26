#=======================================================================================================
#                            printfevd.R
#=======================================================================================================
# This function reshapes the Forecast Error Variance Decomposition such that the object has the following
# dimension: [HORIZON x VARIABLE x SHOCK]
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================

printfevd <- function(xfevd) {
  
# Set dimensions for the reshape  
FHORZ <- dim(xfevd)[3]  
EPS <- dim(xfevd)[2]
KVAR <- dim(xfevd)[1]

# Assign labels
# Endogenous variables
if ( is.null(dimnames(xfevd)[[1]]) == FALSE ) {
  
  labVAR <- dimnames(xfevd)[[1]]
  
} else {
  
  labVAR <- paste0('y', 1:KVAR)
  
}

# Forecast horizon
if ( is.null(dimnames(xfevd)[[3]]) == FALSE ) {
  
  labHORZ <- dimnames(xfevd)[[3]]
  
} else {
  
  labHORZ <- paste0('hor', 1:FHORZ)
  
}

# Shocks
if ( is.null(dimnames(xfevd)[[2]]) == FALSE ) {
  
  labEPS <- dimnames(xfevd)[[2]]
  
} else {
  
  labEPS <- paste0('eps', 1:EPS)
  
}

# Reshape the FEVD
pfevd <- array(0, dim = c(FHORZ, KVAR, EPS) , dimnames = list(labHORZ, labVAR, labEPS) ) # [FHORZ x KVAR x EPS]
 
for (jj in 1 : EPS) {
  
  for (kk in 1 : KVAR) {
    
    for (hh in 1 : FHORZ) {
      
       pfevd[hh,kk,jj] <- xfevd[kk,jj,hh]
      
    }
    
  }

}

return(pfevd)

}



