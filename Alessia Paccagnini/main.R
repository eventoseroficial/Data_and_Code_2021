#=======================================================================================================
#                               main.R
#=======================================================================================================
# This script replicates the example in Kilian and Lutkepohl (2017), Structural Vector Autoregressive
# Analysis, Cambridge University Press, Chapter 9 (page 239-240).
#
# The exercise focuses on the identification of an oil price shock and its impact on inflation and 
# real GDP. 
#
# The oil price shock is identified by using a recursive scheme (that is through Cholesky
# decomposition of the VAR's reduced form residuals covariance matrix).
# 
# These R codes allow to compute impulse response functions (IRFs), Forecast Error Variance 
# Decomposition (FEVD) and Historical Decomposition (HD).
# 
# Author: Alessia Paccagnini (UCD) and Fabio Parla (CBI)
# contact: fabioparla123@gmail.com
# Dublin, May 2021
# 
# Disclaimer: The views expressed in these teaching materials are those of the authors and do not reflect 
# the views of the Central Bank of Ireland or the ESCB. Any errors are our own.
#=======================================================================================================


# 1. Setting work directory, installing packages and importing functions

# 1.1 Setting work directory 
wd <- 'C:\\Users\\aless\\SER\\' # or alternatively one can set the wd manually

# Setting work directory
setwd(wd)

# 1.2 Updating packages
source(paste0(wd, 'packages\\packages.R'))

# 1.3 Updating functions
file.sources <- list.files(paste0(wd, '\\functions') , pattern="*.R$", full.names=TRUE, ignore.case=FALSE)
sapply(file.sources, source, .GlobalEnv)
#-------------------------------------------------------------------------------------------------------

# 2. Import data and visualizing series
# Time series used in the exercise:
#
# RPOIL = percent changes in the real WTI price of crude oil
# INFL  = U.S. GDP deflator inflation rate
# GDP   = real GDP growth

# 2.1. Import data and labels
DATAFULL <- read.csv(paste0(wd, 'data\\dataoil1.csv'), header = TRUE) 

# 2.2 Plot endogenous variables
dates <- seq(as.Date(DATAFULL$date[1], format = "%d/%m/%Y"), as.Date(DATAFULL$date[length(DATAFULL$date)],
                                                                     format = "%d/%m/%Y"), "quarter")  # Change if data are at a different frequency, e.g. monthly

NAMES <- c('Real WTI price of crude oil', 'GDP Deflator inflation', 'Delta Real GDP') # Labels of the 
# three endogenous variables

plotvar <- list()

for (ii in 1:length(NAMES)) {  
  
  DATAPLOT <- data.frame(DATES=dates, SERIES=DATAFULL[,ii+1])
  
  P0 <- ggplot(DATAPLOT, aes(DATES,SERIES)) + geom_line(colour="black", size = 1) + theme_classic() + 
    labs(y = 'Percent', x = 'Years') + ggtitle(NAMES[ii]) + theme(plot.title = element_text(hjust = 0.5)) + 
    geom_hline(yintercept=0, color = "black", size=0.1) 
  
  plotvar[[ii]] <- ggplotGrob(P0)
  
}
dev.new()
do.call(grid.arrange , c(plotvar,ncol=2))
#-------------------------------------------------------------------------------------------------------

# 3. Settings and VAR specification 
# Notes.The Structural form representation of the VAR is:
#
# y{t} = A{1} y{t-1} + A{2} y{t-2} + A{3} y{t-3} + A{4} y{t-4} + c + B{0}e{t}         
# 
# where y{t} is the 3 x 1 vector of of endogenous variables and A{p}, for p = 1,...,4, is a  3 × 3 slopes
# coefficients matrix.  
# Moreover, the relationship between reduced form residuals (u{t}) and the structural shocks (e{t}) is:
#
# u{t} = B{0})e{t}
#
# where u{t} ~ N(0,SIGMA) are the 3 x 1 vector of reduced form residuals, e{t} ~ N(0,I) are the 3 x 1 
# vector of structural disturbances and B{0} is the 3 x 3 matrix contains the contemporaneous effects of
# the structural shocks on the endogenous variables.

# 3.1 Model setup
constant <- 1      # set 1 if a constant is included in the model, otherwise set 0
p <- 4             # number of lags
horz       <- 20   # 20-step ahead forecast horizon (for both IRFs and FEVD) 
cumulative <- TRUE # TRUE or FALSE
scalefactor <- 0   # either 0 (one std shock) or 1 (unitary shock)
CONFBANDS <- 0.90  # confidence intervals, otherwise NULL if compute only point estimate
ndr <- 200         # number of bootstrap replications, otherwise NULL if compute only point estimate
shock <- 1         # Shock of interest (to be used later in the plot function)


# 4. OLS estimation: point estimate and confidence intervals

# 4.1 Data used in the estimation
DATA <- as.matrix(DATAFULL[,-1])  #remove column with dates 

# 4.2 Retrieve info for OLS estimation
K <- ncol(DATA)
tobs <- nrow(DATA)
tess <- tobs-p

# 4.3 Get data matrices for estimation
# Re-write the VAR in Matrix notation:
#
# Y = X A + U
#
# where Y is the T x 3 matrix of contemporaneous endogenous variables, X is a [T x (3 x p + 1)] matrix 
# of lagged endogenous variables plus the intercept, A is a [(3 x p + 1) x 3] matrix of slopes 
# coefficients and U is a T x 3 matrix of reduced form residuals.
#
# Notes. Each column in A contains the slopes coefficients from the VAR equations (i.e. the 1st column 
# contains the parameters from the 1st equation, the  2nd column contains the parameters from the 2nd 
# equation, and so on ...)

Y <- DATA[(1+p):tobs , ]
X <- getlag(DATA,p,constant)
ncoeff <- ncol(X) # Number of coefficients for each VAR equation

# 4.4 Get OLS quantities 
capA <- solve(t(X)%*%X) %*% t(X)%*%Y       
u <- Y - X%*%capA
sse <- crossprod(u)
SIGMA <- (1 / (tess-ncoeff)) * sse      


# 5 Structural VAR analysis 
# 5.1 Short-run restriction via Cholesky decomposition of the residual variance-covariance matrix (SIGMA)
B0 <- t(chol(SIGMA))

# 5.2 Structural Impulse Response Functions
IRF <- getirf(capA,p,B0,horz,scalefactor,cumulative) # Get point estimates 

IRFBOOT <- getirfbands(DATA,p,constant,capA,horz,cumulative,scalefactor,CONFBANDS,ndr) # Get the median and confidence intervals

# 5.3 Forecast Error Variance Decomposition (FEVD)
FEVD <-  getfevd(capA,B0,p,horz) # Get point estimates

FEVDBOOT <- getfevdbands(DATA,p,constant,capA,B0,horz,CONFBANDS,ndr)  # Get the median and confidence intervals


# 5.4 Historical decomposition
HD <- gethd(DATA,capA,B0,u,constant,p)


# 6. Plot VAR tools

# 6.1 Plot IRF
IRFY <- IRF[,,shock] # point estimate of the responses of interest (to be plotted together with confidence bands)
dev.new()
plotirf(IRFBOOT,IRFY,NAMES,shock) 

# 6.2 Plot FEVD
FEVDY <- FEVD[,,shock] # point estimate of the responses of interest (to be plotted together with confidence bands)
dev.new()
plotfevd(FEVDBOOT,FEVDY,NAMES,shock) 

# 6.3 Plot Historical Decomposition (HD)
dev.new()
plothd(dates,p,DATA,HD,shock,NAMES)
