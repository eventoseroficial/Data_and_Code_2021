#=======================================================================================================
#                      getcomp.R 
#=======================================================================================================
# This function construct the companion matrix containing the slopes coefficients obtained from the 
# estimation of a VAR(p).
# See slides page
# 
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================

getcomp <- function(Beta,P) {

nvar <- ncol(Beta)

BB <- t(Beta)

# Remove deterministic terms
CAPB <- BB[, 1:(nvar*P)]  

# Get Companion matrix
Fupp <- CAPB
Flw <- cbind(diag(nvar*(P-1)), matrix(0 , nrow = nvar*(P-1), ncol = nvar))

capF <- rbind(Fupp, Flw) 

return(capF)

}