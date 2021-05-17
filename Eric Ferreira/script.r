##########################################
# V SER: Famila ExpDes (9-10 Jun 2021)
##########################################

library('ExpDes.pt')

##########################################
# GExpDes
##########################################
# Git: https://github.com/gExpDes/gexpdes
# Sevidor: https://gexpdes.ufsc.br
##########################################

##########################################
# Experimento Mangarito
##########################################

## Sabor ##
#Carregando os dados:
s<-read.csv2("sabor.csv",h=T,sep=";")
View(s)

a<-with(s,
    dbc(preparo,
      consumidor,
      sabor,
      quali=TRUE,
      mcomp="tukey",
      sigF=0.05,
      sigT=0.05
      ))

#Tratamentos
m<-tapply(s$sabor,s$preparo,mean)
se<-tapply(s$sabor,s$preparo,sd)/sqrt(50)
c<-barplot(m, ylim=c(0,10), density=30, 
           col=c("blue",'orange',"blue","blue"),
           ylab='Sabor',xlab='Mode de Preparo')
text(c,m+se,c('b','a','b','b'),pos=3)
error.bar(c,m,se,col='red')

#Análise de resíduos
plotres(a)

## L da casca ##
L<-read.csv2("Lcasca.csv",h=T,sep=";",dec=',')
View(L)

a<-with(L,
        fat2.dic(tempo,
            temperatura,
            Lcasca,
            fac.names=c("Tempo","Temperatura"),
            quali=c(FALSE,TRUE),
            mcomp="tukey",
            sigF=0.05,
            sigT=0.05
        ))

#Gráfico
par(mfrow=c(1,1))
x<-0:28
y<-53.42114286-0.81527891*x+0.01272595*x^2
xob<-c(0,7,21,14,28)
yob<-c(52.99667,49.86833,43.10833,42.45667,40.31333)
plot(x,y,'l',xlim=c(0,28),ylim=c(30,60),
     xlab="Tempo (dias)", ylab="L* casca",
     col='red')
points(xob,yob,pch=20)
legend("bottomleft",legend=c(expression(paste(
  'y = 53,4 - 0.82x + 0.013',x^2)),
  expression(paste(R^2,' = 93%'))), cex=0.8,bty='n',
  text.col="red")
