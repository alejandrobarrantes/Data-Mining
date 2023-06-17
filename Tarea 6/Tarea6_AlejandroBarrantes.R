library("FactoMineR") 
library("factoextra")
library("cluster")
library("fmsb")
library("traineR")
library(tidyverse)
library(ggplot2)

# Ejercicio 1=================================================================
# EJERCICIO #1
#|    MC             |             | Modelo        | Modelo       |
#|-------------------|-------------|---------------|--------------|
#|                   | Matriz      | NO            | SI           |
#| REAL              | NO          | 4 (VN)        | 3  (FP)      |
#| REAL              | SI          | 2 (FN)        | 16 (VP)      |
VN<-4
FN<-2
FP<-3
VP<-16

#1.	Precision global:
P<-(VN+VP)/(VN+FP+FN+VP)
P
#2. Error global:  
E<-(1-P)
E
#3.	Precisión positiva:
PP<- VP/(FN+VP)
PP
#4.	Precisión negativa:
PN<-VN/(VN+FP)
PN
#5.	Proporción de falsos positivos: 
PFP<- FP/(VN+FP)
PFP
#6.	Proporción de falsos negativos: 
PFN<-FN/(FN+VP)
PFN
#7.	Asertividad positiva: 
AP <- VP/(FP+VP) 
AP
#8.	Asertividad negativa: 
AN <- VN/(VN+FN)
AN

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

# Ejercicio 2 ======================================================================================
# EJERCICIO #2

calcular_metricas <- function(matriz_confusion) {
  
  # Extraer los valores de la matriz de confusión
  VN <- matriz_confusion[1,1]  # verdaderos positivos
  FN <- matriz_confusion[2,1]  # falsos negativos
  FP <- matriz_confusion[1,2]  # falsos positivos
  VP <- matriz_confusion[2,2]  # verdaderos negativos
  
  # Calcular las métricas de rendimiento
  precision_global <- (VN+VP)/(VN+FP+FN+VP)
  error_global <- 1 - precision_global
  precision_positiva <- VP/(FN+VP)
  precision_negativa <- VN/(VN+FP)
  falsos_positivos <- FP/(VN+FP)
  falsos_negativos <- FN/(FN+VP)
  asertividad_positiva <- VP/(FP+VP)
  asertividad_negativa <- VN/(VN+FN)
  
  # Crear una lista con las métricas de rendimiento
  metricas <- list(precision_global = precision_global,
                   error_global = error_global,
                   precision_positiva = precision_positiva,
                   precision_negativa = precision_negativa,
                   falsos_positivos = falsos_positivos,
                   falsos_negativos = falsos_negativos,
                   asertividad_positiva = asertividad_positiva,
                   asertividad_negativa = asertividad_negativa)
  
  # Devolver la lista con las métricas de rendimiento
  return(metricas)
}

matriz_confusion <- matrix(c(892254, 212, 8993, 300), nrow=2, byrow=TRUE)
matriz_confusion
metricas <- calcular_metricas(matriz_confusion)
metricas

# La precisión global del modelo es alta (0.989), lo que indica que el modelo clasifica correctamente la mayoría de los casos. Sin embargo, la precisión positiva es muy baja (0.032), lo que significa que el modelo tiene una tasa muy alta de falsos negativos, es decir, que hay muchos casos positivos que el modelo clasifica como negativos.

# Además, la tasa de falsos positivos es muy baja (0.0002), lo que indica que el modelo tiene una buena capacidad para evitar clasificar casos negativos como positivos. La asertividad positiva es baja (0.58), lo que indica que el modelo acierta menos de la mitad de las veces cuando predice que un caso es positivo. La asertividad negativa es alta (0.99), lo que indica que el modelo acierta la gran mayoría de las veces cuando predice que un caso es negativo.

# En conclusión, el modelo predictivo tiene una alta precisión global, pero tiene una tasa muy alta de falsos negativos, lo que puede ser problemático en algunos contextos. Por lo tanto, en general, el modelo no es bueno o malo por sí solo, sino que su desempeño debe ser evaluado cuidadosamente en función del contexto en el que se está utilizando.


# Ejercicio 3======================================================================================
# EJERCICIO #3
equilibrio.variable.predecir <- function(datos, variable.predecir, ylab = "Cantidad de individuos", 
                                         xlab = "", main = paste("Distribución de la variable",variable.predecir), col = NA) {
  gg_color <- function (n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  if(missing(variable.predecir) | !(variable.predecir %in% colnames(datos))){
    stop("variable.predecir tiene que ser ingresada y ser un nombre de columna", call. = FALSE )
  }
  if(is.character(datos[,variable.predecir]) | is.factor(datos[,variable.predecir])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(datos[,variable.predecir])))
    }else{
      col <- rep(col,length(unique(datos[,variable.predecir])))
    }
    ggplot(data = datos, mapping = aes_string(x = variable.predecir, fill = variable.predecir)) +
      geom_bar() +
      scale_fill_manual(values = col, name = variable.predecir) +
      labs(x = xlab, y = ylab, title = main) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }else{
    stop("La variable a predecir tienen que ser de tipo factor o character", call. = FALSE )
  }
}

poder.predictivo.numerica <- function(datos, variable.predecir, variable.comparar, ylab = "", 
                                      xlab = "", main = paste("Densidad de la variable", variable.comparar, 'según', variable.predecir), col = NA){
  gg_color <- function (n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  if(missing(variable.predecir) | !(variable.predecir %in% colnames(datos))){
    stop("variable.predecir tiene que ser ingresada y ser un nombre de columna", call. = FALSE )
  }
  if(missing(variable.comparar) | !(variable.comparar %in% colnames(datos)) | !is.numeric(datos[,variable.comparar])){
    stop("variable.comparar tiene que ser ingresada y ser un nombre de columna numérica", call. = FALSE )
  }
  
  if(is.character(datos[,variable.predecir]) | is.factor(datos[,variable.predecir])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(datos[,variable.predecir])))
    }else{
      col <- rep(col,length(unique(datos[,variable.predecir])))
    }
    
    ggplot(data = datos, aes_string(variable.comparar, fill = variable.predecir)) +
      geom_density(alpha = .7, color = NA) +
      scale_fill_manual(values = col) +
      labs(title = main , y = ylab, x = xlab ,fill = variable.predecir) +
      theme_minimal() +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            text = element_text(size = 15))
    
  }else{
    stop("La variable a predecir tienen que ser de tipo factor o character", call. = FALSE )
  }
}

poder.predictivo.categorica <- function(datos, variable.predecir, variable.comparar, ylab = "", 
                                        xlab = "", main = paste("Densidad de la variable", variable.comparar, 'según', variable.predecir), col = NA) {
  gg_color <- function (n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  if(missing(variable.predecir) | !(variable.predecir %in% colnames(datos))){
    stop("variable.predecir tiene que ser ingresada y ser un nombre de columna", call. = FALSE )
  }
  if(missing(variable.comparar) | !(variable.comparar %in% colnames(datos)) | 
     !(is.factor(datos[,variable.comparar]) | is.character(datos[,variable.comparar])) ){
    stop("variable.comparar tiene que ser ingresada y ser un nombre de columna categórica", call. = FALSE )
  }
  
  if(is.character(datos[,variable.predecir]) | is.factor(datos[,variable.predecir])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(datos[,variable.predecir])))
    }else{
      col <- rep(col,length(unique(datos[,variable.predecir])))
    }
    
    datos2 <- datos %>%
      dplyr::group_by_(variable.comparar, variable.predecir) %>%
      dplyr::summarise(count = n())
    
    if(variable.comparar != variable.predecir){
      datos2 <-   datos2 %>% dplyr::group_by_(variable.comparar)
    }
    datos2 <- datos2 %>% dplyr::mutate(prop = round(count/sum(count),4))
    
    ggplot(data = datos2, mapping = aes_string(x = variable.comparar, y = "prop", fill = variable.predecir)) +
      geom_col(position = "fill") +
      geom_text(aes(label = glue("{percent(prop)} ({count})")), position = position_stack(vjust = .5), color = "white") +
      scale_y_continuous(label = percent) +
      labs(y =  xlab, x  = ylab, title = main) +
      scale_fill_manual(values = col, name = variable.predecir) +
      theme(legend.position = "bottom")+
      coord_flip()
    
  }else{
    stop("La variable a predecir tienen que ser de tipo factor o character", call. = FALSE )
  }
}
# Índices para matrices NxN
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

#Cargando tabla de datos 
Datos <- read.csv('Tumores.csv', header=TRUE, sep=',',dec = '.',stringsAsFactors = T)
#Datos$tipo <- factor(Datos$tipo)
str(Datos)

equilibrio.variable.predecir(Datos,"tipo")

#25% de los datos para testing y el 75% para aprendizaje
numero.filas <- dim(Datos)[1]
muestra <- sample(1:numero.filas,numero.filas*0.25)
ttesting <- Datos[muestra, ]
taprendizaje <- Datos[-muestra, ]

modelo<-train.knn(tipo~.,data=taprendizaje,kmax=floor(sqrt(numero.filas)))

prediccion <- predict(modelo, ttesting, type = "class")
head(prediccion$prediction)

MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)



#Rectangular
modelo.rectangular<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "rectangular")
modelo.rectangular

#Prediccion 
prediccion <- predict(modelo.rectangular, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)


#Triangular
modelo.triangular<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "triangular")
modelo.triangular

#Prediccion 
prediccion <- predict(modelo.triangular, ttesting, type = "class")
head(prediccion$prediction)

#Epanechnikov
modelo.epanechnikov<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "epanechnikov")
modelo.epanechnikov
#Prediccion 
prediccion <- predict(modelo.epanechnikov, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)

#=================
#Biweight
modelo.biweight<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "biweight")
modelo.biweight

#Prediccion 
prediccion <- predict(modelo.biweight, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)

# Índices de Calidad de la predicción
general.indexes(mc = MC)


#Triweight
modelo.triweight<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "triweight")
modelo.triweight

#Prediccion 
prediccion <- predict(modelo.rectangular, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)

# Índices de Calidad de la predicción
general.indexes(mc = MC)


#Cos
modelo.cos<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "cos")
modelo.cos

#Prediccion 
prediccion <- predict(modelo.cos, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)

# Índices de Calidad de la predicción
general.indexes(mc = MC)

#Inv
modelo.inv<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "inv")
modelo.inv

#Prediccion 
prediccion <- predict(modelo.inv, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)


#Gaussian
modelo.gaussian<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "gaussian")
modelo.gaussian

#Prediccion 
prediccion <- predict(modelo.gaussian, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)

#Optimal
modelo.optimal<- train.knn(tipo~., data = taprendizaje, kmax=floor(sqrt(numero.filas)), kernel = "optimal")
modelo.optimal

#Prediccion 
prediccion <- predict(modelo.optimal, ttesting, type = "class")
head(prediccion$prediction)

#Matriz de confusion
MC <- confusion.matrix(ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = MC)


# Ejercicio 4======================================================================================
# EJERCICIO #4

# Ejercicio 5======================================================================================
# EJERCICIO #5

#5.1
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 6/Datos Clase y Tareas")
Datos <- read.table("ZipData_2020.csv", header=TRUE, sep=';',dec='.', stringsAsFactors = T)

#5.2
table(Datos$Numero)

#Si es equilibrado, ya que tenemos cantidades parecidas de cada categoria

#5.3
tam<-dim(Datos)
n<-tam[1]
muestra <- sample(1:n,floor(n*0.20))
ttesting <- Datos[muestra,]
taprendizaje <- Datos[-muestra,]

# train.kknn escoje el k usando leave-one-out crossvalidation
modelo<-train.knn(Numero~.,data=(taprendizaje),kmax=floor(sqrt(n)))

prediccion <- predict(modelo, ttesting, type = "class")
MC<-confusion.matrix(ttesting, prediccion)
general.indexes(mc=MC)

#Se obtiene una precision del 95%, lo que significa que 
#los resultados son muy buenos, ademas de que en los resultados de 
#precision por categoria se mantienen arriba del 90%, lo cual es muy buen 
#numero 