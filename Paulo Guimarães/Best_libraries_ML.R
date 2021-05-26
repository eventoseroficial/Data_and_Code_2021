
### Script - Best R Libraries for Machine Learning

# Example - Dataset iris

data(iris) # conjunto de dados iris

library(dplyr)
glimpse(iris) # informações sobre o conjunto  de dados

summary(iris) # resumo

# Conjunto de treinamento e teste

library(caTools)

set.seed(1) # semente

divisao <- sample.split(iris$Species, SplitRatio = 0.75) # divisão do conjunto em 75% para treinamento e 25% para teste
divisao

treinamento <- subset(iris, divisao == TRUE)
treinamento

teste <- subset(iris, divisao == FALSE)
teste

### Árvores de Decisão

library(rpart)

classificador1 <- rpart(formula =  Species ~ ., data = treinamento)
print(classificador1)
plot(classificador1)
text(classificador1)

library(partykit)
plot(as.party(classificador1)) # outra forma de mostrar a árvore

previsoes1<- predict(classificador1, newdata = teste[-5], type = 'class')
matriz_confusao1 = table(teste[, 5], previsoes1) # matriz de confusão
print(matriz_confusao1)

library(caret)
confusionMatrix(matriz_confusao1)

### Random Forest

library(randomForest)

classificador2<-randomForest(x = treinamento[-5], y = treinamento$Species, ntree = 30)
previsoes2<-predict(classificador2, newdata = teste[-5])
matriz_confusao2<-table(teste[, 5], previsoes2)
print(matriz_confusao2)

confusionMatrix(matriz_confusao2)

# importância da variável

imp<-randomForest(Species ~ ., data = treinamento,importance = TRUE)
imp
varImpPlot(imp,type = 1)
varImpPlot(imp,type = 2)

### K-NN

library(class)
previsoes3<-knn(train = treinamento[, -5], test = teste[, -5],
                cl = treinamento[, 5], k = 5)

matriz_confusao3<-table(teste[,5], previsoes3)
print(matriz_confusao3)

confusionMatrix(matriz_confusao2)

# usando escalonamento (importante quando há discrepância entre as variáveis)

dados<-iris

dados[, 1:4] = scale(dados[, 1:4])
dados


### Aprendizagem bayesiana

library(e1071)

classificador4<-naiveBayes(x = treinamento[-5], y = treinamento$Species)
print(classificador4) # usando o método Naive-Bayes

previsoes4<-predict(classificador4,newdata = teste[-5])
matriz_confusao4<-table(teste[,5],previsoes4)
confusionMatrix(matriz_confusao4)

### SVM

library(e1071)

classificador5<-svm(formula = Species ~ ., data = treinamento, type = 'C-classification',
                    kernel = 'radial') # escolher o kernel

previsoes5<-predict(classificador5, newdata = teste[-5])
matriz_confusao5<-table(teste[,5], previsoes5)

library(caret)
confusionMatrix(matriz_confusao5)

plot(classificador5, data=iris,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4))

### Redes neurais

library(neuralnet)
classificador6 <- neuralnet(Species ~ .,
                           data=treinamento, hidden=c(10,10), rep = 5, err.fct = "ce", 
                           linear.output = F, lifesign = "minimal", stepmax = 1000000,
                           threshold = 0.001) # configurações da rede neural


plot(classificador6, rep ="best") # plotar a rede neural

prev <- compute(classificador6, teste[-5])
idx <- apply(prev$net.result, 1, which.max)
previsoes6 <- c('setosa', 'versicolor', 'virginica')[idx]

matriz_confusao6<-table(teste[,5], previsoes6)
confusionMatrix(matriz_confusao6)

### Xbosting

library(xgboost)
library(dplyr)

head(iris)

library(caTools)

set.seed(1)

divisao <- sample.split(iris$Species, SplitRatio = 0.75)
divisao

treinamento <- subset(iris, divisao == TRUE)
treinamento

teste <- subset(iris, divisao == FALSE)
teste

y_treino <- as.integer(treinamento$Species) - 1 # definição de treino e teste
y_teste <- as.integer(teste$Species) - 1
X_treino <- treinamento %>% select(-Species)
X_teste <- teste %>% select(-Species)


xgb_treino <- xgb.DMatrix(data = as.matrix(X_treino), label = y_treino)
xgb_teste <- xgb.DMatrix(data = as.matrix(X_teste), label = y_teste)

xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss", 
  num_class = length(levels(iris$Species))) # configurações 


xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_treino,
  nrounds = 5000,
  verbose = 1) # configurando o modelo
xgb_model 

xgb_prev <- predict(xgb_model, as.matrix(X_teste), reshape = TRUE)
xgb_prev <- as.data.frame(xgb_prev)
colnames(xgb_prev) <- levels(iris$Species)
xgb_prev 

xgb_prev$PredictedClass <- apply(xgb_prev, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_prev$ActualClass <- levels(iris$Species)[y_teste + 1]
xgb_prev # previsões

library(caret)

confusionMatrix(factor(xgb_prev$ActualClass), factor(xgb_prev$PredictedClass))

### Validação cruzada

library(caret)

controle_treinamento<-trainControl(method = 'repeatedcv', number = 10, repeats = 2)
modelo<- train(Species ~., data = dados, trControl = controle_treinamento, method = 'rf')
print(modelo)
acuracia<- modelo$results$Accuracy[2]
acuracia


########### Pacote rattle

library(rattle)
rattle()
