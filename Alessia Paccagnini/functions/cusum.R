#=======================================================================================================
#                              cusum.R 
#=======================================================================================================
# This function computes cumulative sum of an array
#
# Notes. It is used if the IRFs are computed as a cumulative sum of the original responses
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


cusum <- function(xarr) {
  
H <- dim(xarr)[3] 
n <- nrow(xarr)

# Create array of cumulative sums
xsum <- array(0, dim=c(n, n, H), dimnames = dimnames(xarr))
xsum[,,1] <- xarr[,,1] # Initialize
  
for (hh in 2 : H) {
   
   xsum[,,hh] <- xarr[,,hh] + xsum[,,hh-1]  
  
}

return(xsum)

}

