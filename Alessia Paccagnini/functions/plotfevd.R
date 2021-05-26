#=======================================================================================================
#                  plotfevd.R
#=======================================================================================================
# This function plots the Forecast Error Variance Decomposition 
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


plotfevd <- function(CAPFEVD,FEVDPE,labnames,shock) {

fhor <- dim(CAPFEVD)[1]    
k <- dim(CAPFEVD)[3] 

plotlist <- list()

for (ii in 1 : k) {  
  
  lbp <- labnames[ii]
  
  DF <- data.frame(0:(fhor-1),CAPFEVD[,,ii,shock],FEVDPE[,ii])
  names(DF) <- c("STEP" ,"LOWER", "MEDIAN", "UPPER","P.ESTIMATE")
  
  P0 <- ggplot(DF , aes(STEP, DF[,5])) +  geom_line(colour="blue", size = 1) + 
        geom_ribbon(data=DF ,aes(ymin=LOWER, ymax=UPPER), alpha=0.4, fill = 'grey') + theme_classic() + 
        labs(y = '', x = 'Quarters') + ggtitle(paste0('Contribution to FEV of ',labnames[ii])) + 
        theme(plot.title = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) + 
        geom_hline(yintercept=0, color = "black", size=0.2) 
  
  plotlist[[ii]] <- ggplotGrob(P0)
  
}

do.call(grid.arrange , c(plotlist,ncol=2))

}

