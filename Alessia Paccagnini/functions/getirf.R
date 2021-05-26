#=======================================================================================================
#                                getirf.R 
#=======================================================================================================
# This function computes point estimates of the Impulse response functions (IRF)
# 
# Notes. For details on the construction of IRFs, see for example Arias et al., (2018)
#
# References
# Arias, J. E., Rubio-Ramírez, J. F., & Waggoner, D. F. (2018). Inference based on structural vector
# autoregressions identified with sign and zero restrictions: Theory and applications. Econometrica, 
# 86(2), 685-720.
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


getirf <- function(capB,L,invA0,horiz,SCALEFACTOR,CUMULAT) {

# Setting information
kvar <- ncol(capB) 
ncf  <- nrow(capB) 

# Create diagonal matrix useful for normalization
if (SCALEFACTOR == 0) {
  
  matscale <- diag(kvar)
  
} else if (SCALEFACTOR == 1) {
  
  matscale <- matrix(0, nrow = kvar , ncol = kvar)
  diag(matscale) <-   diag(1/invA0)
  
}

# Get companion
MATCOMP <- getcomp(capB,L) 

# Selection matrices
J <- rbind( diag(kvar) , matrix(0, nrow = (kvar*L)-kvar, ncol = kvar))  

# Save IRFs 
irf <- array(0, dim = c(kvar, kvar, horiz+1), dimnames = list(NULL , paste0('eps', 1:kvar), 
                                                              paste0('horz',0:horiz)))

if (is.null(invA0)) {
  
  for (hh in 1 : (horiz+1)) {
    
    irf[,,hh] <- t(J) %*% (MATCOMP%^%(hh-1)) %*% J 
    
  }  
  
} else {
  
  for (hh in 1 : (horiz+1)) {
    
    irf[,,hh] <- (t(J) %*% (MATCOMP%^%(hh-1)) %*% J %*% invA0) %*% matscale
    
  }  
  
}    

# If cumulative ...
if (CUMULAT == FALSE) {
  
  irf0 <- irf
  
} else if (CUMULAT == TRUE) {
  
  irf0 <- cusum(irf)
  
}

IRF <- array(0, dim = c(horiz+1, kvar, kvar), dimnames = list(paste0('H=', 0:(horiz)), paste0('y',1:kvar), paste0('eps', 1:kvar)))    # define array to store IRF

for (ii in 1:kvar) {
  
  IRF[,,ii] <- t(irf0[,ii,]) # [HORIZONS x VARIABLES x EPSILONS]
  
}   

return(IRF)

}
