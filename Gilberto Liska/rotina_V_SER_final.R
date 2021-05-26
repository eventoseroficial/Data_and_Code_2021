## rotina para análise via POT para janeiro
#mudar diretorio (coloque o caminho do local onde esta o conjunto de dados)
setwd("E:/Documents/UFSCar/Cursos_e_Eventos/2021_V_SIR")

#limpar memoria
rm(list=ls(all=TRUE))

#carregar dados
library(openxlsx)# carregar pacote
dados=read.xlsx("dados_jan.xlsx",
                sheet=1,colNames = TRUE) # 
head(dados)
summary(dados)
attach(dados)

Y1=dados$Precipitacao; Y1

#############################################################################
###############       Analise via metodologia POT          ##################
#############################################################################

################   graficos para a escolha do threshold    ##################
library(evd)
## grafico 1: Mean Residual life plot
mrlplot(Y1,main="",
        col = c("green","black","green")) ## grafico da media dos excedentes
## grafico 2: threshold choice plot
par(mfrow=c(1,2))
tlim = c(0,100)
tcplot(Y1,tlim,std.err = FALSE) ## grafico para escolha do threshold

ts<-40 ## coloque aqui o threshold!!

############   organizar serie acima do threshold     #######################
dadosmod<-Y1[Y1>ts]
length(dadosmod) 

############   teste de Ljung Box para independencia da serie    ############
Box.test(dadosmod,type=c("Ljung-Box"))

### teste de tendência de Mann-Kendall
require(trend)
mk.test(dadosmod) ##pegar o p-value

########### ajuste da distribuicao Generalizada de Pareto  ##################
z1=fpot(Y1,ts,shape=0); z1 # ajusta os parametros da dist generalizada de pareto
z1
pos1<-z1$threshold; pos1
esc1<-z1$estimate[1]; esc1

z1x=fpot(Y1,ts); z1x
pos12<-z1x$threshold; pos12
esc12<-z1x$estimate[1]; esc12
sha12<-z1x$estimate[2]; sha12

############      teste da razao de verossimilhancas       ##################
anova(z1x,z1)    # faz o teste da razao de verossimilhanca.

############       teste de Kolmogorov Smirnov        #######################
ks.test(dadosmod,"pgpd",pos1,esc1,alternative="two.sided")

ks.test(dadosmod,"pgpd",pos12,esc12,sha12,alternative="two.sided")

############       graficos de diagnostico       ############################
plot(z1,which = 2, main="January (Exponential)", xlab="Model", 
     ylab="Empirical", las=1, lwd=1, cilwd=1) ##QQ plot

par(mfrow=c(2,2))
plot(z1) ##QQ plot

############       Probabilidades            ################################
n<-c(50, 75, 100, 125, 150)
# prob Exponencial
round(pgpd(n,pos1, esc1, lower.tail=FALSE),4)
# prob GPD
round(pgpd(n,pos12, esc12, sha12, lower.tail=FALSE),4)

#######################################################################
## Encontrando os tempos de retorno para 2,5, 10, 30, 50 e 100 anos: ##
#######################################################################
x<-c(2, 5, 10, 30, 50, 100)                                                             ## Tempos de retorno armazenados no vetor x
prob<-1-(1/x)                                                                     ## Probabilidades para cada um dos tempos de retorno do vetor x
# retorno Exponencial
retorno1<-qgpd(prob,loc=pos1,scale=esc1)#12 é o n de meses
round(retorno1,2)         ## tempos de retornos previstos
# retorno GPD
retorno2<-qgpd(prob,loc=pos12,scale=esc12,shape=sha12)#12 é o n de meses
round(retorno2,2)         ## tempos de retornos previstos

ret_jan_TS=data.frame(tempo=x,previsao=retorno1)
ret_jan_TS

##############################################################################
##           Montando os graficos para cada mes                             ##
##############################################################################

##################################################
### inicio da rotina para contrucao do IC  #######
### para Exponencial (generalizada Pareto) #######
##################################################

####### nivel de retorno estimado
## pag 82 livro coles2011
# p.ret: tempo de retorno
# ajuste: objeto com ajuste da GPD (caso csi=0)
# ny: numero de observações do periodo analisado (ex.: se for mes => ny=31 ou 30; se for ano => n)

nrest.exp=function(p.ret, ajuste, ny){
  aj=ajuste
  sigma<-as.numeric(aj[[1]][1:1])
  mu<-aj$threshold
  zeta<-aj$nhigh/aj$npp
  
  xr=numeric(length(p.ret))
  xr=mu+sigma*log((p.ret*ny*zeta))
  return(xr)
}
nrest.exp(x, z1, ny=31)

##Variancia do nivel de retorno estimado 
#(valido para 1 observacao)
#pg 82 livro coles2011, equacao 4.15
VarNRet.Exp=function(p.ret, ajuste, ny){
  aj=ajuste
  sigma<-as.numeric(aj[[1]][1:1])
  mu<-aj$threshold
  zeta<-aj$nhigh/aj$npp
  
  xr=numeric(length(p.ret))
  xr=mu+sigma*log((p.ret*ny*zeta))
  #matriz de var e cov
  MVC=matrix(0,2,2)
  varsigma<-aj$var.cov[1]
  
  varzeta=zeta*(1-zeta)/aj$npp
  
  MVC[1,1]=varzeta
  MVC[2,2]=varsigma
  MVC[1,2]=0;MVC[2,1]=0
  
  ##vetor gradiente
  f=quote(mu+sigma*log((p.ret*ny*zeta)))
  df.sigma<-D(f,"sigma")   #derivada em relacao a sigma
  df.zeta<-D(f,"zeta")     #derivada em relacao a zeta
  
  dsigma=eval(df.sigma, list(mu=mu,sigma=sigma,
                             p.ret=p.ret,zeta=zeta))
  dzeta=eval(df.zeta, list(mu=mu,sigma=sigma,
                           p.ret=p.ret,zeta=zeta))
  
  Vgrad=c(dzeta, dsigma)
  Varret=t(Vgrad)%*%MVC%*%Vgrad
  return(Varret)
}
VarNRet.Exp(x[1], z1x, ny=31)

### matriz com os resultados 
Res.Exp=function(p.ret, ajuste, ny){
  aux=matrix(0,length(p.ret),5)
  prob=numeric(length(p.ret))
  for(i in 1:length(p.ret))
  {
    aux[i,1]=p.ret[i]
    aux[i,2]=nrest.exp(aux[i,1], ajuste, ny)
    
    ## fornece os mesmos resultados de aux[i,2]
    #prob[i]<-1-(1/aux[i,1]) 
    #aux[i,2]=qgpd(prob[i],loc=ajuste$threshold,
    #              scale=as.numeric(ajuste[[1]][1:1]))
    
    aux[i,3]=VarNRet.Exp(aux[i,1], ajuste, ny)
    aux[i,4]=aux[i,2]-2*1.96*sqrt(aux[i,3])
    aux[i,5]=aux[i,2]+2*1.96*sqrt(aux[i,3])
  }
  aux2=data.frame(tempo=aux[,1],xt=aux[,2], Varxt=aux[,3],
                  LI=aux[,4], LS=aux[,5])
  return(aux2)
}

###
### mudar aqui o ny!!
tab2=Res.Exp(x, z1, ny=31)
tab2

plot(tab2$tempo,tab2$xt,type ='b', 
     pch=19, col = "red", lwd=2, xaxt="n",
     main='January (Exponential)', xlab='Time of return (years)',
     ylab='Return level of rainfall (mm)',
     ylim=c(0,250), panel.first=grid(),bty="L")
axis(1, at=c(2, 5, 10, 30, 50, 100), 
     lab=expression(2, 5, 10, 30, 50, 100))
lines(tab2$tempo,tab2$LI,type ='b',lwd=2,lty=2,col="red")
lines(tab2$tempo,tab2$LS,type ='b',lwd=2,lty=2,col="red")
