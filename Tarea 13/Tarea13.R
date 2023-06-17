library(snow)
library(traineR)
library(caret)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(glue)
library(tidyverse)
library(scales)
library(class)
library(e1071)
library(randomForest)


# Funciones generales


plotROC <- function(prediccion, real, adicionar = FALSE, color = "red") {
  pred <- ROCR::prediction(prediccion, real)    
  perf <- ROCR::performance(pred, "tpr", "fpr")
  plot(perf, col = color, add = adicionar, main = "Curva ROC")
  segments(0, 0, 1, 1, col='black')
  grid()  
}



# Ejercicio #1

# Lectura


setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 7/Datos Clase y Tareas")
Datos <- read.csv("Tumores.csv", header=TRUE, sep=',',dec='.',stringsAsFactors = T)
enteros <- sapply(Datos, is.integer)
Datos[enteros] <- lapply(Datos[enteros], as.factor)

Datos$tipo <- factor(Datos$tipo)
Datos <- subset(Datos, select = -1)

set.seed(123) # Fijamos la semilla para reproducibilidad
indice <- createDataPartition(y = Datos$tipo, p = 0.25, list = FALSE)
ttesting <- Datos[indice, ]
ttraining <- Datos[-indice, ]


## 1.1



#====================================
#svm
modelo <- train.svm(tipo ~ ., data = ttraining)
prediccion <- predict(modelo, ttesting, type = "prob")

Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase)



#====================================
#knn
modelo <- train.knn(tipo ~ ., data = ttraining, kmax = tam)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "blue")


#====================================
#arboles
modelo <- train.rpart(tipo ~ ., data = ttraining)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "green")


#====================================
#bosques
modelo <- train.randomForest(tipo ~ ., data = ttraining)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "yellow")


#====================================
#potenciacion
modelo <- train.ada(tipo ~ ., data = ttraining)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "orange")

#====================================
#xgboost
modelo <- train.xgboost(tipo ~ ., data = ttraining, nrounds = 500)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "lightblue")


#====================================
#lda
modelo <- train.lda(tipo ~ ., data = ttraining)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "lightgray")

#====================================
#bayes
modelo <- train.bayes(tipo ~ ., data = ttraining)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "brown")


#====================================
#nnet
modelo <- train.nnet(tipo ~ ., data = ttraining, size = 4, maxit = 1000, MaxNWts = 400, trace = FALSE)
prediccion <- predict(modelo, ttesting, type = "prob")


Score <- prediccion$prediction[,2]
Clase <- ttesting$tipo
# Genera el gráfico
plotROC(Score,Clase, adicionar=TRUE, color = "purple")

