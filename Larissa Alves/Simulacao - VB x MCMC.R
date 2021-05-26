# Automatic relevance determination model (Drugowitsch 2019)

# Modelo ARD:
# y     ~ N(X*beta, phi^(-1)*I_n)
# beta  ~ N(0, (phi*D_tau)^(-1))  D_tau = diag(tau_1, ...,tau_p)
# phi   ~ Ga(a0, b0)
# tau_j ~ Ga(c0,d0)   j = 1,..., p

###########################################################
# Pacotes
install.packages('MASS')
install.packages('R2jags')
install.packages('tictoc')
library(MASS)
library(invgamma)
library(LaplacesDemon)
library(Rmpfr)
library(R2jags)
library(tictoc)

###########################################################
# Gerando dados 
n = 10000
p = 10

set.seed(12345)                                  # Semente
phiv  = 0.4 
tauv  = rep(1,p)
betav = mvrnorm(1,rep(0,p),solve(phiv*diag(tauv)),tol = 1e-6, empirical = FALSE)
X <- cbind(replicate(p, rnorm(n,0,1)))           # Gerando X
y <- X %*% betav  + rnorm(n, sd = sqrt(1/phiv))  # Gerando y
#hist(y)

###########################################################
# Variational Bayes

# Guardando em:
it = 50000
c_tau_vetor       = c() 
d_tau_vetor       = c() 
a_phi_vetor       = c() 
b_phi_vetor       = c() 
mu_beta_vetor     = c() 
sigma_beta_vetor1 = array(NA,dim=c(p,p,it))

# Priori phi
a0 = 0.1
b0 = 0.1
# Priori tau_j
c0 = 1
d0 = 0.1

#Valores iniciais
c_tau      = 1 
d_tau      = matrix(1,nrow=p,ncol=1)
a_phi      = 1
b_phi      = 1
mu_beta    = matrix(1,nrow=p,ncol=1)
sigma_beta = matrix(1,nrow=p,ncol=p)

c_tau_vetor            = c_tau
d_tau_vetor            = d_tau
a_phi_vetor            = a_phi 
b_phi_vetor            = b_phi 
mu_beta_vetor          = mu_beta 
sigma_beta_vetor1[,,1] = sigma_beta

tic()

for(i in 2:it)
{
  # PASSO 1: (beta,phi) 
  sigma_beta = solve(diag(c(c_tau/d_tau),p)  + t(X)%*%X)
  mu_beta    = sigma_beta%*%t(X)%*%y 
  a_phi      = a0 + (n/2) 
  b_phi      = b0 + (1/2)*(t(y)%*%y - (t(mu_beta)%*%solve(sigma_beta)%*%mu_beta))
  
  
  # PASSO 2: (tau)
  c_tau         = c0 + 1/2
  for(j in 1:p)
  {
  d_tau[j,1]    = d0 + (1/2)*(((mu_beta[j,1]^2)*(a_phi/b_phi)) + sigma_beta[j,j])
  }
  
  c_tau_vetor            = c(c_tau_vetor, c_tau) 
  d_tau_vetor            = cbind(d_tau_vetor,d_tau) 
  a_phi_vetor            = c(a_phi_vetor,a_phi) 
  b_phi_vetor            = c(b_phi_vetor,b_phi) 
  mu_beta_vetor          = cbind(mu_beta_vetor, mu_beta)
  sigma_beta_vetor1[,,i] = sigma_beta  
    
  if(abs(mu_beta_vetor[,i]-mu_beta_vetor[,i-1]) < 0.0001 && abs(d_tau_vetor[,i]-d_tau_vetor[,i-1]) < 0.0001 && abs(sigma_beta_vetor1[,,i]-sigma_beta_vetor1[,,i-1]) < 0.0001)
  {
    it = i
    break
  }
}  

time       = toc() 
time_vb  = time$toc - time$tic # time

sigma_beta_vetor = c()
sigma_beta_vetor = sigma_beta_vetor1[,,1:it]

sd_beta = matrix(NA,nrow=p,ncol=1)
for(l in 1:p)
{
  sd_beta[l,1] = sqrt((b_phi_vetor[it]/(a_phi_vetor[it]-1))*sigma_beta_vetor[l,l,it])
} 

###########################################################
# MCMC no Jags

###########################################################
# Definições (controladas pelo usuários) 

itera = 5000  #numero total de iterações
cad   = 1      #número de cadeias em paralelo que ele vai rodar 
burn  = 1000   #burnin a ser retirado, somar o numero total planejado + burnin
thin  = 4      # espaçamento

###########################################################
# Definições do Jags 
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
  
  for (j in 1:10) {
    tau[j]  ~ dgamma(1,0.1)
    beta[j] ~ dnorm(0,phi*(tau[j]))
    }
}
'

model_parameters =  c('beta',"phi",'tau')

amostra <- cbind(y,X)

n = dim(amostra)[1] 

model_data = list(n = n, y = amostra[,1], X = amostra[,2:(p+1)]) 

tic()

# Aqui utilizamos a função de fato
model = jags(data = model_data, 
             parameters.to.save = model_parameters, 
             model.file=textConnection(model_code), 
             n.chains=cad,  #numero de cadeias
             n.iter=itera,  #numero de iterações
             n.burnin=burn, #aquecimento
             n.thin=thin,   #espaçamento
             DIC=FALSE)

time       = toc() # End timer
time_mcmc  = time$toc - time$tic # time

print(model)
amostras<-model$BUGSoutput$sims.matrix # Extraindo as amostras

###########################################################
# Convergência
library(coda)
raftery.diag(amostras[,1]) 

###########################################################
# Cadeias
{
plot(amostras[,1],type='l')
abline(h=betav[1],col='red')

plot(amostras[,2],type='l')
abline(h=betav[2],col='red')

plot(amostras[,3],type='l')
abline(h=betav[3],col='red')

plot(amostras[,4],type='l')
abline(h=betav[4],col='red')

plot(amostras[,5],type='l')
abline(h=betav[5],col='red')

plot(amostras[,6],type='l')
abline(h=betav[6],col='red')

plot(amostras[,7],type='l')
abline(h=betav[7],col='red')

plot(amostras[,8],type='l')
abline(h=betav[8],col='red')

plot(amostras[,9],type='l')
abline(h=betav[9],col='red')

plot(amostras[,10],type='l')
abline(h=betav[10],col='red')

plot(amostras[,11],type='l')
abline(h=phiv,col='red')

plot(amostras[,12],type='l')
abline(h=tauv[1],col='red')

plot(amostras[,13],type='l')
abline(h=tauv[2],col='red')

plot(amostras[,14],type='l')
abline(h=tauv[3],col='red')

plot(amostras[,15],type='l')
abline(h=tauv[4],col='red')

plot(amostras[,16],type='l')
abline(h=tauv[5],col='red')

plot(amostras[,17],type='l')
abline(h=tauv[6],col='red')

plot(amostras[,18],type='l')
abline(h=tauv[7],col='red')

plot(amostras[,19],type='l')
abline(h=tauv[8],col='red')

plot(amostras[,20],type='l')
abline(h=tauv[9],col='red')

plot(amostras[,21],type='l')
abline(h=tauv[10],col='red')
}

###### INTERVALOS HPD
{
betav

HPDinterval(as.mcmc(amostras[,1]))
HPDinterval(as.mcmc(amostras[,2]))
HPDinterval(as.mcmc(amostras[,3]))
HPDinterval(as.mcmc(amostras[,4]))
HPDinterval(as.mcmc(amostras[,5]))
HPDinterval(as.mcmc(amostras[,6]))
HPDinterval(as.mcmc(amostras[,7]))
HPDinterval(as.mcmc(amostras[,8]))
HPDinterval(as.mcmc(amostras[,9]))
HPDinterval(as.mcmc(amostras[,10]))

phiv
HPDinterval(as.mcmc(amostras[,11]))
}


# Gráficos
# Beta
plot(function(x)dt((x-mu_beta_vetor[1,it])/sd_beta[1,],2*a_phi_vetor[it])/sd_beta[1,],min(amostras[,1]),max(amostras[,1]),col=2,lwd=3,ylab="density",xlab=expression(beta[1]),main="",cex.axis=1.4,cex.lab=1.7)
hist(amostras[,1],freq=FALSE,add=TRUE)
plot(function(x)dt((x-mu_beta_vetor[1,it])/sd_beta[1,],2*a_phi_vetor[it])/sd_beta[1,],min(amostras[,1]),max(amostras[,1]),col=2,lwd=3,add=TRUE)
points(betav[1],0.001,col=3,lwd=10)

# Phi
plot(function(x)dgamma(x,a_phi_vetor[it],b_phi_vetor[it]),min(amostras[,11]),max(amostras[,11]),col=2,lwd=3,ylab="density",xlab=expression(phi),main="",cex.axis=1.4,cex.lab=1.7)
hist(amostras[,11],freq=FALSE,add=TRUE)
plot(function(x)dgamma(x,a_phi_vetor[it],b_phi_vetor[it]),min(amostras[,11]),max(amostras[,11]),col=2,lwd=3,add=TRUE)
points(phiv,0.001,col=3,lwd=10)

# Tau
plot(function(x)dgamma(x,c_tau_vetor[it],d_tau_vetor[1,it]),0,max(amostras[,12]),col=2,lwd=3,ylab="density",xlab=expression(tau[1]),main="",cex.axis=1.4,cex.lab=1.7)
hist(amostras[,12],freq=FALSE,add=TRUE)
plot(function(x)dgamma(x,c_tau_vetor[it],d_tau_vetor[1,it]),0,max(amostras[,12]),col=2,lwd=3,add=TRUE)
points(tauv[1],0.001,col=3,lwd=10)
