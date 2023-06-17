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

#====== Funciones

precision<-function(clase){
  function(mc){
    print(clase)
    indices=general.indexes(mc=mc)
    indices$category.accuracy[clase]
  }
}

precision.global<-function(x) sum(diag(x))/sum(x)

error.global <- function(x) 1 - sum(diag(x))/sum(x)

#========= LEER DATOS
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 7/Datos Clase y Tareas")
Datos <- read.csv("Tumores.csv", header=TRUE, sep=',',dec='.',stringsAsFactors = T)
enteros <- sapply(Datos, is.integer)
Datos[enteros] <- lapply(Datos[enteros], as.factor)

Datos$tipo <- factor(Datos$tipo)
Datos <- subset(Datos, select = -1)

set.seed(123) # Fijamos la semilla para reproducibilidad
indice <- createDataPartition(y = Datos$tipo, p = 0.25, list = FALSE)
# ttesting <- Datos[indice, ]
# ttraining <- Datos[-indice, ]
str(Datos)


###========================================================
peones <- parallel::detectCores()
clp <- makeCluster(peones, type = "SOCK")

# Constructor del cluster
clusterExport(clp, "Datos")

ignore <- clusterEvalQ(clp, {
  library(traineR)
  
  ejecutar.prediccion <- function(datos, formula, muestra,metodo, ...) {
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    modelo <- metodo(formula, data = taprendizaje, ...)
    prediccion <- predict(modelo, ttesting, type = "class")
    MC <- confusion.matrix(ttesting, prediccion)
    return(MC)
  }
  return(NULL)
})

numero.filas <- nrow(Datos)
cantidad.grupos <- 10
cantidad.validacion.cruzada <- 5
algoritmos <- c("discrete", "real", "gentle")

deteccion.si.discrete <- c()
deteccion.si.real <- c()
deteccion.si.gentle <- c()

# Para medir el tiempo de ejecución
tiempo.paralelo <- Sys.time()

for(i in 1:cantidad.validacion.cruzada) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos)
  si.discrete <- 0
  si.real <- 0
  si.gentle <- 0
  
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]
    
    ### Inserta estas 1 variable en cada peón
    clusterExport(clp, "muestra")
    
    resultado <- clusterApply(clp, algoritmos, function(pmetodo) {
      MC <- ejecutar.prediccion(Datos, tipo ~ .,muestra, train.ada , type = pmetodo, iter=80, nu=1)
      no.val <- MC[2, 2]
      print(pmetodo)
      valores <- list(Tipo = pmetodo, Resultado = no.val)
      valores
    })
    
    for (j in 1:length(algoritmos)) {
      if (resultado[[j]][[1]] == "discrete") 
        si.discrete <- si.discrete + resultado[[j]][[2]] 
      else if (resultado[[j]][[1]] == "real")
        si.real <- si.real + resultado[[j]][[2]] 
      else if (resultado[[j]][[1]] == "gentle")
        si.gentle <- si.gentle + resultado[[j]][[2]] 
    }
  }
  
  deteccion.si.discrete[i] <- si.discrete
  deteccion.si.real[i] <- si.real
  deteccion.si.gentle[i] <- si.gentle
}

stopCluster(clp) # No olvidar cerrar el proceso

# Despliega el tiempo que tarda en ejecutarse
tiempo.paralelo <- Sys.time() - tiempo.paralelo
resultado


# GRAFICACION
resultados <- data.frame("discrete"     = deteccion.si.discrete,
                         "real"     = deteccion.si.real,
                         "gentle" = deteccion.si.gentle) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Detección del 1's tipo en ada", 
        xlab = "Número de iteración",
        ylab = "Cantidad de 1's tipo detectados",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda




#########    1.2 

###========================================================
peones <- parallel::detectCores()
clp <- makeCluster(peones, type = "SOCK")

# Constructor del cluster
clusterExport(clp, "Datos")

ignore <- clusterEvalQ(clp, {
  library(traineR)
  
  ejecutar.prediccion <- function(datos, formula, muestra,metodo, ...) {
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    modelo <- metodo(formula, data = taprendizaje, ...)
    prediccion <- predict(modelo, ttesting, type = "class")
    MC <- confusion.matrix(ttesting, prediccion)
    return(MC)
  }
  return(NULL)
})

numero.filas <- nrow(Datos)
cantidad.grupos <- 10
cantidad.validacion.cruzada <- 5
algoritmos <- c("discrete", "real", "gentle")

deteccion.error.discrete <- c()
deteccion.error.real <- c()
deteccion.error.gentle <- c()

# Para medir el tiempo de ejecución
tiempo.paralelo <- Sys.time()

for(i in 1:cantidad.validacion.cruzada) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos)
  error.discrete <- 0
  error.real <- 0
  error.gentle <- 0
  
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]
    
    ### Inserta estas 1 variable en cada peón
    clusterExport(clp, "muestra")
    
    resultado <- clusterApply(clp, algoritmos, function(pmetodo) {
      MC <- ejecutar.prediccion(Datos, tipo ~ .,muestra, train.ada , type = pmetodo, iter=80, nu=1)
      error.metodo <- (1-(sum(diag(MC)))/sum(MC))*100
      valores <- list(Tipo = pmetodo, Error = error.metodo)
      valores
    })
    
    for (j in 1:length(algoritmos)) {
      if (resultado[[j]][[1]] == "discrete") 
        error.discrete <- error.discrete + resultado[[j]][[2]] 
      else if (resultado[[j]][[1]] == "real")
        error.real <- error.real + resultado[[j]][[2]] 
      else if (resultado[[j]][[1]] == "gentle")
        error.gentle <- error.gentle + resultado[[j]][[2]] 
    }
  }
  
  deteccion.error.discrete[i] <- error.discrete/ cantidad.grupos
  deteccion.error.real[i] <- error.real/ cantidad.grupos
  deteccion.error.gentle[i] <- error.gentle/ cantidad.grupos
}

stopCluster(clp) # No olvidar cerrar el proceso

# Despliega el tiempo que tarda en ejecutarse
tiempo.paralelo <- Sys.time() - tiempo.paralelo
resultado


# GRAFICACION
resultados <- data.frame("discrete"     = deteccion.error.discrete,
                         "real"     = deteccion.error.real,
                         "gentle" = deteccion.error.gentle) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda




#########    1.3

###========================================================
peones <- parallel::detectCores()
clp <- makeCluster(peones, type = "SOCK")

# Constructor del cluster
clusterExport(clp, "Datos")

ignore <- clusterEvalQ(clp, {
  library(traineR)
  
  ejecutar.prediccion <- function(datos, formula, muestra,metodo, ...) {
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    modelo <- metodo(formula, data = taprendizaje, ...)
    prediccion <- predict(modelo, ttesting, type = "class")
    MC <- confusion.matrix(ttesting, prediccion)
    return(MC)
  }
  return(NULL)
})

numero.filas <- nrow(Datos)
cantidad.grupos <- 10
cantidad.validacion.cruzada <- 5
algoritmos <- c("discrete", "real", "gentle")

MCs.discrete <- list()
MCs.real <- list()
MCs.gentle <- list()

# Para medir el tiempo de ejecución
tiempo.paralelo <- Sys.time()

for(i in 1:2) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos)
  MC.discrete <- matrix(c(0,0,0,0), nrow=2)
  MC.real <- matrix(c(0,0,0,0), nrow=2)
  MC.gentle <- matrix(c(0,0,0,0), nrow=2)
  
  
  for(k in 1:2) {
    muestra <- grupos[[k]]
    
    ### Inserta estas 1 variable en cada peón
    clusterExport(clp, "muestra")
    MC.metodo <- 0
    
    resultado <- clusterApply(clp, algoritmos, function(pmetodo) {
      MC <- ejecutar.prediccion(Datos, tipo ~ .,muestra, train.ada , type = pmetodo, iter=80, nu=1)
      #MC.metodo <- MC+MC.metodo
      valores <- list(Tipo = pmetodo, MC = MC)
      valores
    })
    
    for (j in 1:length(algoritmos)) {
      if (resultado[[j]][[1]] == "discrete") 
        MC.discrete <- MC.discrete + resultado[[j]][[2]] 
      else if (resultado[[j]][[1]] == "real")
        MC.real <- MC.real + resultado[[j]][[2]] 
      else if (resultado[[j]][[1]] == "gentle")
        MC.gentle <- MC.gentle + resultado[[j]][[2]] 
    }
  }
  
  MCs.discrete[[i]] <- MC.discrete
  MCs.real[[i]] <- MC.real
  MCs.gentle[[i]] <- MC.gentle
}

stopCluster(clp) # No olvidar cerrar el proceso


# GRAFICACION 1's
resultados <- data.frame("discrete"     = sapply(MCs.discrete,precision("1")),
                         "real"     = sapply(MCs.real,precision("1")),
                         "gentle" = sapply(MCs.gentle,precision("1"))) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda


# GRAFICACION 0's
resultados.0 <- data.frame("discrete"     = sapply(MCs.discrete,precision("0")),
                         "real"     = sapply(MCs.real,precision("0")),
                         "gentle" = sapply(MCs.gentle,precision("0"))) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda


general.indexes(mc=MC.discrete)





# GRAFICACION Error global
resultados.error <- data.frame("discrete"     = sapply(MCs.discrete,error.global),
                           "real"     = sapply(MCs.real,error.global),
                           "gentle" = sapply(MCs.gentle,error.global)) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados.error, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados.error),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados.error)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados.error),bty='n', xpd=NA,
       pch=1:ncol(resultados.error), col = rainbow(ncol(resultados.error))) # La leyenda




