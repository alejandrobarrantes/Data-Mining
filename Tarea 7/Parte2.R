library(ggplot2)
library(dplyr)
library(glue)
library(tidyverse)
library(scales)
library(traineR)
library(caret)
library(kknn)

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




#----- 
#EJERCICIO #2  --- Compartir

#2.1 Cargue la tabla de datos titanicV2020.csv, asegurese re-codificar las variables cualitativas y 
#de ignorar variables que no se deben usar.

setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 7/Datos Clase y Tareas")
datos <- read.table("titanicV2020.csv", header=TRUE, sep=',',dec='.',stringsAsFactors = T)

# Eliminamos las columnas: 
datos <- subset(datos, select = -c(PassengerId,Name,SibSp,Parch,Ticket, Cabin))
datos<-na.omit(datos)
col_enteras <- sapply(datos, is.integer)
datos[col_enteras] <- lapply(datos[col_enteras], as.factor)


#-----
#2.2 Usando el comando sample de R genere al azar una tabla aprendizaje con un 80 % de los
#datos y con el resto de los datos genere una tabla de aprendizaje.

muestra <- sample(nrow(datos), floor(0.8 * nrow(datos)))

taprendizaje <- datos[muestra,]
ttesting <- datos[-muestra,]

#-----
#2.3 Genere un Modelo Predictivo usando SVM, con el paquete traineR, luego para este modelo
#calcule la matriz de confusi´on, la precisi´on, la precisi´on positiva, la precisi´on negativa, los
#falsos positivos, los falsos negativos, la acertividad positiva y la acertividad negativa.
#Utilice el Kernel que d´e mejores resultados.

#svm
svm <- train.svm(Survived~., data = taprendizaje, kernel = "sigmoid")
prediccion <- predict(svm, ttesting, type = "class")


MC1 <- confusion.matrix(ttesting, prediccion)
general.indexes(mc=MC1)
calcular_metricas(MC1)

#----------
#2.4 Construya una tabla para los ´ındices anteriores que permita comparar el resultado de los
#m´etodos SVM con respecto a los m´etodos de las tareas anteriores ¿Cu´al m´etodo es mejor?

#knn
KNN<-train.knn(Survived~.,data=taprendizaje,kmax=floor(sqrt(nrow(datos))))
prediccion <- predict(KNN, ttesting, type = "class")
MC2 <- confusion.matrix(ttesting, prediccion)
(general.indexes(mc = MC2))


#kknn
modelo.kknn<-train.kknn(Survived~.,data=taprendizaje,kmax=floor(sqrt(nrow(datos))))
prediccion <- predict(modelo.kknn, ttesting[,-1])
MC3 <- table(ttesting$Survived, prediccion)




df_metricas <- data.frame()

metricas<- c(calcular_metricas(MC1))
metricas$Modelo <-"svm"
df_metricas <- rbind(df_metricas,metricas)


metricas<- c(calcular_metricas(MC2))
metricas$Modelo <-"knn"
df_metricas <- rbind(df_metricas,metricas)


metricas<- c(calcular_metricas(MC3))
metricas$Modelo <-"kknn"
df_metricas <- rbind(df_metricas,metricas)



df_metricas


#El mejor metodo es el knn