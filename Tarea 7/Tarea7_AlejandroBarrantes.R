library(ggplot2)
library(dplyr)
library(glue)
library(tidyverse)
library(scales)
library(traineR)
library(caret)
library(class)
library(e1071)
library(kknn)
#==================================================
#FUNCIONES
#==================================================
calcular_metricas <- function(MC) {
  
  VN <- MC[1,1]  
  FN <- MC[2,1] 
  FP <- MC[1,2]  
  VP <- MC[2,2]  
  PG <- (VN+VP)/(VN+FP+FN+VP)
  EG <- 1 - PG
  PP <- VP/(FN+VP)
  PN <- VN/(VN+FP)
  AP <- VP/(FP+VP)
  AN <- VN/(VN+FN)
  
  metricas <- list(precision_global = PG,
                   error_global = EG,
                   precision_positiva = PP,
                   precision_negativa = PN,
                   falsos_positivos = FP,
                   falsos_negativos = FN,
                   asertividad_positiva = AP,
                   asertividad_negativa = AN)
  
  return(metricas)
}

tabla.comparacion<-function(matrices){
  resultados <- data.frame(Modelo = character(),
                           Precisión_Global = numeric(),
                           Error_Global = numeric(),
                           Falsos_Positivos = numeric(),
                           Falsos_Negativos = numeric(),
                           Precisión_Positiva = numeric(),
                           Precisión_Negativa = numeric(),
                           Asertividad_Positiva = numeric(),
                           Asertividad_Negativa = numeric(),
                           stringsAsFactors = FALSE)
  
  
  for (name in names(matrices)) {
    
    indices.generales<-general.indexes(mc=matrices[[name]])
    metricas<-calcular_metricas(matrices[[name]])
    
    if (!name %in% resultados$Modelo){
      nueva_fila <- data.frame(Modelo = name,
                               Precisión_Global = indices.generales$overall.accuracy,
                               Error_Global = indices.generales$overall.error,
                               Precisión_Positiva = metricas$precision_positiva,
                               Precisión_Negativa = metricas$precision_negativa,
                               Falsos_Positivos = metricas$falsos_positivos,
                               Falsos_Negativos = metricas$falsos_negativos,
                               Asertividad_Positiva = metricas$asertividad_positiva,
                               Asertividad_Negativa = metricas$asertividad_negativa
      )
      
      resultados <- rbind(resultados, nueva_fila)
    }
  }
  return(resultados)
}
#===========================================
#EJERCICIO #1

#---------
#1.1
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 7/Datos Clase y Tareas")
Datos <- read.table("Tumores.csv", header=TRUE, sep=',',dec='.',stringsAsFactors = T)
enteros <- sapply(Datos, is.integer)
Datos[enteros] <- lapply(Datos[enteros], as.factor)

set.seed(123) # Fijamos la semilla para reproducibilidad
indice <- createDataPartition(y = Datos$tipo, p = 0.25, list = FALSE)
ttesting <- Datos[indice, ]
taprendizaje <- Datos[-indice, ]

#---------
#1.2

tabla.comparacion <- data.frame()

#Linear
modelo <- train.svm(tipo~., data = taprendizaje, kernel = "linear")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
unlist(general.indexes(mc=MC))
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"svm-linear"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#radial
modelo <- train.svm(tipo~., data = taprendizaje, kernel = "radial")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"svm-radial"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#Polynomial
modelo <- train.svm(tipo~., data = taprendizaje, kernel = "polynomial")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
unlist(general.indexes(mc=MC))
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"svm-polynomial"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#Sigmoid
modelo <- train.svm(tipo~., data = taprendizaje, kernel = "sigmoid")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
unlist(general.indexes(mc=MC))
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"svm-sigmoid"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#---------
#1.3


tam <- floor(sqrt(nrow(Datos))) 


#Rectangular
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "rectangular")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-rectangular"
tabla.comparacion <- rbind(tabla.comparacion,metricas)



#Triangular
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "triangular")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-triangular"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#Epanechnikov
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "epanechnikov")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-epanechnikov"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#Biweight
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "biweight")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-biweight"
tabla.comparacion <- rbind(tabla.comparacion,metricas)


#Triweight
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "triweight")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-triweight"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

#Cos
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "cos")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-cos"
tabla.comparacion <- rbind(tabla.comparacion,metricas)


#Inv
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "inv")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-inv"
tabla.comparacion <- rbind(tabla.comparacion,metricas)


#Gaussian
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "gaussian")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-gaussian"
tabla.comparacion <- rbind(tabla.comparacion,metricas)


#Optimal
modelo<- train.knn(tipo~., data = taprendizaje, kmax=tam, kernel = "optimal")
prediccion <- predict(modelo, ttesting, type = "class")
MC <- confusion.matrix(ttesting, prediccion)
metricas<- c(calcular_metricas(MC))
metricas$Modelo <-"knn-optimal"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

tabla.comparacion <- select(tabla.comparacion, Modelo, everything())

tabla.comparacion


#=====================================================================================


#EJERCICIO #2
#2.1
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 7/Datos Clase y Tareas")
Datos <- read.table("titanicV2020.csv", header=TRUE, sep=',',dec='.',stringsAsFactors = T)

# Eliminamos las columnas PassengerId,Name,SibSp,Parch,Ticket, Cabin, Embarked: 
Datos <- subset(Datos, select = -c(PassengerId,Name,SibSp,Parch,Ticket, Cabin, Embarked))
#omitimos nulos
Datos<-na.omit(Datos)
#recodificamos
enteros <- sapply(Datos, is.integer)
Datos[enteros] <- lapply(Datos[enteros], as.factor)

#--------------------------------------
#2.2
muestra <- sample(nrow(Datos), floor(0.8 * nrow(Datos)))
taprendizaje <- Datos[muestra,]
ttesting <- Datos[-muestra,]

#--------------------------------------
#2.3
#modelo con SVM
modelo.svm <- svm(Survived~., data = taprendizaje, kernel = "sigmoid")
prediccion <- predict(modelo.svm, ttesting, type = "class")
MC1 <- table(ttesting$Survived, prediccion)
calcular_metricas(MC1)

#--------------------------------------
#2.4
#modelo con train.knn

modelo.train<-train.knn(Survived~.,data=taprendizaje, kmax=floor(sqrt(nrow(Datos))))
prediccion <- predict(modelo.train, ttesting, type = "class")
MC2 <- confusion.matrix(ttesting, prediccion)


#modelo con train.kknn
modelo.kknn<-train.kknn(Survived~.,data=taprendizaje,kmax=floor(sqrt(nrow(Datos))))
prediccion <- predict(modelo.kknn, ttesting[,-1])
MC3 <- table(ttesting$Survived, prediccion)

#tabla de comparacion de valores
tabla.comparacion <- data.frame()

metricas<- c(calcular_metricas(MC1))
metricas$Modelo <-"SVM"
tabla.comparacion <- rbind(tabla.comparacion,metricas)


metricas<- c(calcular_metricas(MC2))
metricas$Modelo <-"KNN"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

metricas<- c(calcular_metricas(MC3))
metricas$Modelo <-"KKNN"
tabla.comparacion <- rbind(tabla.comparacion,metricas)

tabla.comparacion <- select(tabla.comparacion, Modelo, everything())

#Comparaciones
tabla.comparacion


#--- borrador

# metricas<- c(calcular_metricas(MC))
# metricas$Modelo <-"svm-polynomial"
# tabla.comparacion <- rbind(tabla.comparacion,metricas)

