# Pacotes
install.packages('MASS')
install.packages('R2jags')
library(MASS)
library(R2jags)
library(MASS)
library(invgamma)
library(LaplacesDemon)
library(Rmpfr)
library(coda)
library(tictoc)

# Modelo ARD:
# y     ~ N(X*beta, phi^(-1)*I_n)
# beta  ~ N(0, (phi*D_tau)^(-1))  D_tau = diag(tau_1, ...,tau_p)
# phi   ~ Ga(a0, b0)
# tau_j ~ Ga(c0,d0)   j = 1,..., p

data = read.table("space_data.txt",header=TRUE)
data = subset(data, Mag != 0) # Retirando magnitudes iguais a zero
n   = dim(data)[1]
y   = data[,3]

ind = rep(1,n) 
X   = cbind(ind,data[,1],data[,2],data[,4])
p   = 4 

# Definições 
itera = 5000   # numero total de iterações
cad   = 1       # número de cadeias em paralelo 
burn  = 1000   # burnin a ser retirado, somar o numero total planejado + burnin
thin  = 4      # espaçamento

# Definições do Jags ------------------------------------------------------------------------------------
model_code = '
model
{
  # Vero
  for (i in 1:n) {
  y[i] ~ dnorm(mu[i], phi)
  mu[i] <- X[i,]%*%beta
  }
  
  # Prioris
  phi ~ dgamma(0.1,0.1)
  
  for (j in 1:4) {
    tau[j]  ~ dgamma(1,0.1)
    beta[j] ~ dnorm(0,phi*(tau[j]))
    }
}
'

model_parameters =  c('beta',"phi",'tau')

amostra <- cbind(y,X)

n = dim(amostra)[1] 
p = 4

model_data = list(n = n, y = amostra[,1], X = amostra[,2:(p+1)])

tic()

model = jags(data = model_data,
             parameters.to.save = model_parameters, 
             model.file=textConnection(model_code), 
             n.chains=cad,  
             n.iter=itera,  
             n.burnin=burn, 
             n.thin=thin,   
             DIC=FALSE)

time       = toc() 
time_mcmc  = time$toc - time$tic # time

print(model)
amostras<-model$BUGSoutput$sims.matrix 

raftery.diag(amostras[,2]) 

######### AMOSTRAS

plot(amostras[,1],type='l')

plot(amostras[,2],type='l')

plot(amostras[,3],type='l')

plot(amostras[,4],type='l')

plot(amostras[,5],type='l')

plot(amostras[,6],type='l')

plot(amostras[,7],type='l')

plot(amostras[,8],type='l')

plot(amostras[,9],type='l')

# Gráficos
hist(amostras[,1])
hist(amostras[,2])


###### INTERVALOS HPD

HPDinterval(as.mcmc(amostras[,1]))
HPDinterval(as.mcmc(amostras[,2]))
HPDinterval(as.mcmc(amostras[,3]))
HPDinterval(as.mcmc(amostras[,4]))
HPDinterval(as.mcmc(amostras[,5]))
HPDinterval(as.mcmc(amostras[,6]))
HPDinterval(as.mcmc(amostras[,7]))
HPDinterval(as.mcmc(amostras[,8]))
HPDinterval(as.mcmc(amostras[,9]))

