library("FactoMineR") 
library("factoextra")
library("cluster")
library("fmsb")
library(kknn)

#======================================================

## Funciones generales

indices.general <- function(MC) {
  precision.global <- sum(diag(MC))/sum(MC)
  error.global <- 1 - precision.global
  precision.categoria <- diag(MC)/rowSums(MC)
  res <- list(matriz.confusion = MC, precision.global = precision.global, error.global = error.global, 
              precision.categoria = precision.categoria)
  names(res) <- c("Matriz de Confusión", "Precisión Global", "Error Global", 
                  "Precisión por categoría")
  return(res)
}

#Colores de ggplot2
define.colores <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}  

#Calcula proporciones
dist.x.predecir <- function(data, variable, variable.predecir) {
  data. <- data %>%
    group_by_(variable, variable.predecir) %>%
    summarise(count = n()) %>%
    mutate(prop = round(count/sum(count),4))
  return(data.)
}

#======================================================

# EJERCICIO #5
getwd()
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 6/Datos Clase y Tareas")
Datos <- read.table("ZipData_2020.csv", header=TRUE, sep=';',dec='.')
Datos$Numero<-factor(Datos$Numero)

tam<-dim(Datos)
n<-tam[1]
muestra <- sample(1:n,floor(n*0.20))
ttesting <- Datos[muestra,]
taprendizaje <- Datos[-muestra,]
class(ttesting)

# train.kknn escoje el k usando leave-one-out crossvalidation
modelo<-train.kknn(Numero~.,data=(ttesting),kmax=floor(sqrt(n)))
modelo
prediccion<-predict(modelo,ttesting)
prediccion

MC<-table(ttesting[,1],prediccion)
indices.general(MC)

