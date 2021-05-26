#=======================================================================================================
#                  plothd.R
#=======================================================================================================
# This function plots the contribution of a structural shock along the detrended time series (i.e. 
# removing the deterministic components) 
#
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


plothd <- function(DATES,L,DATAMAT,HD,shock,NAMES) {
  
  
DATESPLOT <- DATES[(1+L):length(DATES)]

capT <- nrow(DATAMAT)  
YTEMP <- DATAMAT[(1+L):capT, ] 
k <- ncol(DATAMAT)

CONTRIB <- HD$HDunexp[,shock,] # Contribution of the shock of interest 


plotlist <- list()

for (ii in 1:k) {
  
  PLOTHD <- data.frame(DATESP=DATESPLOT,DETRENDED=YTEMP[,ii]-t(HD$HDinit)[-1,ii],CONTRIBUTION=CONTRIB[-1,ii])  
  
  P1 <- ggplot() + 
        geom_line(data=PLOTHD, aes(x=DATESP, y=DETRENDED, color="De-trended series"), size=0.8)  +
        geom_line(data=PLOTHD, aes(x=DATESP, y=CONTRIBUTION, color="Contribution shock"), size=0.8) +
        scale_colour_manual(name='', values=c("De-trended series"="black", "Contribution shock"="red")) +
        theme_classic() + labs(y = '', x = 'Years') + 
        ggtitle(paste0('Historical decomposition: ', NAMES[ii])) + 
        theme(plot.title = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) + 
        theme(legend.position="top") + theme(legend.title = element_text(size=18)) + 
        geom_hline(yintercept=0, color = "black", size=0.2)  

  plotlist[[ii]] <- ggplotGrob(P1)
  
}

do.call(grid.arrange , c(plotlist,ncol=2))  


}