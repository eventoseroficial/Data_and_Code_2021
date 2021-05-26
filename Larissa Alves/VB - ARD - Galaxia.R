# VB using automatic relevance determination (Drugowitsch 2019)

# Modelo ARD:
# y     ~ N(X*beta, phi^(-1)*I_n)
# beta  ~ N(0, (phi*D_tau)^(-1))  D_tau = diag(tau_1, ...,tau_p)
# phi   ~ Ga(a0, b0)
# tau_j ~ Ga(c0,d0)   j = 1,..., p

# Pacotes
library(MASS)
library(invgamma)
library(LaplacesDemon)
library(Rmpfr)

# Data
data = read.table("space_data.txt",header=TRUE)
data = subset(data, Mag != 0) # Retirando magnitudes iguais a zero
n   = dim(data)[1]
y   = data[,3]
ind = rep(1,n) 
X   = cbind(ind,data[,1],data[,2],data[,4])
p   = 4 

# Clássico:
reg_model = lm(y~data[,1]+data[,2]+data[,4])
summary(reg_model)


###########################################################
# Variational Bayes

# Guardando em:
it = 10000
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
    
  if(abs(mu_beta_vetor[,i]-mu_beta_vetor[,i-1]) < 0.00001 && abs(d_tau_vetor[,i]-d_tau_vetor[,i-1]) < 0.00001 && abs(sigma_beta_vetor1[,,i]-sigma_beta_vetor1[,,i-1]) < 0.00001)
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


####################################################
# Gráficos:
post_phi   = plot(function(x)dgamma(x,a_phi_vetor[it],b_phi_vetor[it]),0.5,0.7,ylab="Densidade",xlab=expression(phi))
post_tau1  = plot(function(x)dgamma(x,c_tau_vetor[it],d_tau_vetor[1,it]),0,0.03,ylab="Densidade",xlab=expression(tau[1]))
plot(function(x)dt((x-mu_beta_vetor[1,it])/sd_beta[1,],2*a_phi_vetor[it])/sd_beta[1,],33,38,col=2,lwd=3,ylab="density",xlab=expression(beta[1]),main="",cex.axis=1.4,cex.lab=1.7)
plot(function(x)dt((x-mu_beta_vetor[2,it])/sd_beta[2,],2*a_phi_vetor[it])/sd_beta[2,],-10.0e-02,-6.0e-02,col=2,lwd=3,ylab="density",xlab=expression(beta[2]),main="",cex.axis=1.4,cex.lab=1.7)
