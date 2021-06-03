#Carregar a Base de dados
library(readr)
Challenger <- read_csv("C:/Users/ferna/Desktop/Palestra SER/Challenger.csv")

#Alterar tipo de vari�veis 
Challenger$blueWins <- as.factor(Challenger$blueWins)
Challenger$blueFirstBlood <- as.factor(Challenger$blueFirstBlood)
Challenger$blueFirstBaron <- as.factor (Challenger$blueFirstBaron)
Challenger$blueFirstTower <- as.factor (Challenger$blueFirstTower)
Challenger$blueFirstDragon <- as.factor (Challenger$blueFirstDragon)
Challenger$blueFirstInhibitor <- as.factor (Challenger$blueFirstInhibitor)

#Alterando Nomes
Challenger$blueWins<-gsub(0, "Lose", Challenger$blueWins)
Challenger$blueWins<-gsub(1, "Win", Challenger$blueWins)
Challenger$blueFirstTower<-gsub(0, "Red Tower", Challenger$blueFirstTower)
Challenger$blueFirstTower<-gsub(1, "Blue Tower", Challenger$blueFirstTower)
Challenger$blueFirstBaron<-gsub(0, "Blue Baron", Challenger$blueFirstBaron)
Challenger$blueFirstBaron<-gsub(1, "Red Baron", Challenger$blueFirstBaron)
Challenger$blueFirstBlood<-gsub(0, "No", Challenger$blueFirstBlood)
Challenger$blueFirstBlood<-gsub(1, "Yes", Challenger$blueFirstBlood)
Challenger$blueFirstDragon<-gsub(0, "Red Dragon", Challenger$blueFirstDragon)
Challenger$blueFirstDragon<-gsub(1, "Blue Dragon", Challenger$blueFirstDragon)
Challenger$blueFirstInhibitor<-gsub(0, "No", Challenger$blueFirstInhibitor)
Challenger$blueFirstInhibitor<-gsub(1, "Yes", Challenger$blueFirstInhibitor)

#Mostrar Base de dados no RMD
library(DT) 
datatable(Challenger)

#Entendendo Vari�veis
summary(Challenger$gameDuraton)
summary(Challenger$blueKills)
summary(Challenger$redKills)
summary(Challenger$blueAssist)
summary(Challenger$redAssist)
summary(Challenger$blueTotalGold)
summary(Challenger$redTotalGold)

#Particularidades dos Times
par(mfrow = c (1, 2))
tabpizza<-table(Challenger$blueWins)
pie(tabpizza)

tab_ft<-table(Challenger$blueFirstTower)
barplot(tab_ft,
        ylim = c(0,15000),
        col = c("#0080FF", "#B40404"))

tab_drag<-table(Challenger$blueFirstDragon)
barplot(tab_drag,
        ylim = c(0,20000),
        col = c("#0080FF", "#B40404"))

tab_baron<-table(Challenger$blueFirstBaron)
barplot(tab_baron,
        ylim = c(0,20000),
        col = c("#0080FF", "#B40404"))

#Histograma
par(mfrow = c (1, 2))
hist(Challenger$gameDuraton,
     main = "Duração do jogo",
     col = "#8A0886",
     sub = "Maior concentração na altura de 1500",
     ylim = c(0,6000),
     xlim = c(0,3000))
hist(Challenger$blueTotalGold,
     col = "#FF8000",
     main = "Gold Total",
     sub = "Maior concentração altura de 50k",
     ylim = c(0,6000))

#Histograma pt 2
par(mfrow = c (1, 2))
hist(Challenger$blueKills,
     main = "Abates Azul",
     col = "#5858FA",
     sub = "")
hist(Challenger$redKills,
     col = "#FE2E64",
     main = "Abates Vermelho",
     sub = "")


