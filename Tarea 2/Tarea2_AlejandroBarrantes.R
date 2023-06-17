#===================================================
#ERRORES
RSS <- function(Pred,Real) {
  ss <- sum((Real-Pred)^2)
  return(ss)
}

RSE<-function(Pred,Real,NumPred) {
  N<-length(Real)-NumPred-1  # <- length(Real)-(NumPred+1)
  ss<-sqrt((1/N)*RSS(Pred,Real))
  return(ss)
}

MSE <- function(Pred,Real) {
  N<-length(Real)
  ss<-(1/N)*RSS(Pred,Real)
  return(ss)
}

error.relativo <- function(Pred,Real) {
  ss<-sum(abs(Real-Pred))/sum(abs(Real))
  return(ss)
}

#===================================================
#FUNCIONES
library(tidyverse)
library(gpairs)

lower <- function(data, mapping){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(size = 1,col = "dodgerblue3") +
    geom_smooth(method = lm, size = 0.4, color = "red", se = FALSE)
  return(p)
}

diag <- function(data, mapping){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_histogram(bins = 30, col = "black", fill = "#F8766D", aes(y=..density..)) +
    geom_density()
  return(p)
}

indices.precision <- function(real, prediccion,cantidad.variables.predictoras) {
  return(list(error.cuadratico = MSE(prediccion,real),
              raiz.error.cuadratico = RSE(prediccion,real,cantidad.variables.predictoras),
              error.relativo = error.relativo(prediccion,real),
              correlacion = as.numeric(cor(prediccion,real))))
}

plot.real.prediccion <- function(real, prediccion, modelo = "") {
  g <- ggplot(data = data.frame(Real = real, Prediccion = as.numeric(prediccion)), mapping = aes(x = Real, y = Prediccion)) +
    geom_point(size = 1, col = "dodgerblue3") +
    labs(title = paste0("Real vs Predicción", ifelse(modelo == "", "", paste(", con", modelo))),
         x = "Real",
         y = "Predicción")
  return(g)
}

#==============================================================================
#Ejercicio 1

#1.1
getwd()
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 2/Datos_EjemplosClase_Tarea")
datos <- read.table('DeudaCredito.csv',header=TRUE, sep=',',dec='.')

datos2<-subset(datos, select = -c(Estudiante,Genero,Casado,Etnicidad))

indices.training <- sample(nrow(datos2), nrow(datos2)*0.8)
datos.training <- datos2[indices.training, ]

indices.testing <- sample(nrow(datos2), nrow(datos2)*0.2)
datos.testing <- datos2[indices.testing, ]
#La variable a predecir es el balance

#1.2

#1.2.1
library(ellipse)
library(corrplot)

summary(datos)


#1.2.2
matriz.correlacion<-cor(datos2)

corrplot(matriz.correlacion)
#Limite - Balance
#CalifCredit - Balance

#1.2.3

#Atipicos
library(FactoMineR)
library(car)

atipicos<-Boxplot(datos2,data=datos2,id.method="y",col="Blue")
atipicos


#1.3 Basado en las estadısticas basicas explique cual variable num´erica parece ser la mejor para predecir la deuda en tarjeta de cr´edito.

#La mejor seria limite, ya que limite 


#1.4
library(GGally)
ggpairs(select_if(datos.training, is.numeric) %>% dplyr::select(-Balance, everything()), # el select posiciona la variable a predecir al final, para que se grafique bien en caso de que solo haya 1 variable a predecir
        lower = list(continuous = lower), diag = list(continuous = diag))




numero.predictoras <- dim(datos2)[2] - 2
numero.predictoras


modelo.lm<-lm(Balance~.,data=datos.training)
modelo.lm
summary(modelo.lm)


#Coeficientes para Beta
modelo.lm$coefficients
summary(modelo.lm)

#Edad: La edad da un valor absoluto muy bajo hasta menor, ya que tiene muy poco impacto en la variable a 
#predecir, es decir,


#La variable que parece tener mas impacto es Ingreso, ya que con ella tiene el valor absoluto mas grande,
#lo cual sugiere que es una variable importante o con impacto en la variable a predecir






#5.¿Que error se obtiene sobre la tabla de testing para el modelo de regresion lineal? Interprete las medidas de error obtenidas
prediccion <- predict(modelo.lm, datos.testing)
# Medición de precisión
pre.lm <- indices.precision(datos.testing$Balance, prediccion,dim(datos.training)[2])
pre.lm

g <- plot.real.prediccion(datos.testing$Balance, prediccion, modelo = "Regresión Lineal")
g + geom_smooth(method = lm, size = 0.4, color = "red", se = FALSE)


#6. Si tuviera que eliminar alguna o algunas de las variables con la esperanza de que mejore la prediccion 
#¿Cual o cuales de las variables eliminarıa? 
modelo_optimo <- step(modelo.lm, direction = "backward")
summary(modelo_optimo)

#Eliminaria tarjetas, educacion y Edad


#¿El nuevo modelo mejora la prediccion?
nuevo.modelo.lm<-lm(Balance~Ingreso+CalifCredit+Edad,data=datos.training)
summary(nuevo.modelo.lm)

prediccion <- predict(nuevo.modelo.lm, datos.testing)
pre.lm <- indices.precision(datos.testing$Balance, prediccion,3)
pre.lm

#No mejora la prediccion, sin embargo, sigue teniendo una buena correlacion, 
#ya que es mas baja que la correlacion con todas las variables pero por muy poco.

#==========================================================================================

#==========================================================================================

#Ejercicio #2


#2.1 
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 2/Datos_EjemplosClase_Tarea")
datos <- read.table('AsientosNinno.csv',header=TRUE, sep=';',dec='.')
datos


datos2<-datos[,-1] %>%
  mutate(CalidadEstant = recode(CalidadEstant,
                                "Malo" = 1,
                                "Medio" = 2,
                                "Bueno" = 3))
datos2

set.seed(123)
indices.training <- sample(nrow(datos2), nrow(datos2)*0.85)
datos.training <- datos2[indices.training, ]

indices.testing <- sample(nrow(datos2), nrow(datos2)*0.15)
datos.testing <- datos2[indices.testing, ]
datos.testing

#Variable a predecir = Ventas


#2.2.1
summary(datos2)
str(datos2)

#2.2.2
matriz.correlacion<-cor(datos2)
corrplot(matriz.correlacion)
ggpairs(select_if(datos.training, is.numeric) %>% dplyr::select(-Ventas, everything()), # el select posiciona la variable a predecir al final, para que se grafique bien en caso de que solo haya 1 variable a predecir
        lower = list(continuous = lower), diag = list(continuous = diag))


#publicidad y USA
#Ventas y precio

#2.2.3
#Atipicos
atipicos<-Boxplot(datos.training,data=datos.training,id.method="y",col="Blue")
atipicos


#2.3
modelo.lm<-lm(Ventas~.,data=datos.training)
modelo.lm

prediccion <- predict(modelo.lm, datos.training)
# Medición de precisión
numero.predictoras <- dim(datos.training)[2]
numero.predictoras

pre.lm <- indices.precision(datos.training$Ventas, prediccion,numero.predictoras)
pre.lm



#==========================================================================================

#==========================================================================================

#Ejercicio #3

setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 2/Datos_EjemplosClase_Tarea")
datos <- read.table('uscrime.csv',header=TRUE, sep=',',dec='.')


#3.1  Realice un Analisis Exploratorio de los datos con todos los datos
#datos2<-subset(datos, select = -c(CalidadEstant))
datos
summary(datos)
str(datos)
matriz.correlacion<-cor(datos)
corrplot(matriz.correlacion)


atipicos<-Boxplot(datos,data=datos,id.method="y",col="Blue")
atipicos

indices.training <- sample(nrow(datos), nrow(datos)*0.67)
datos.training <- datos[indices.training, ]
datos.training

indices.testing <- sample(nrow(datos), nrow(datos)*0.33)
datos.testing <- datos[indices.testing, ]
datos.testing



modelo.lm<-lm(ViolentCrimesPerPop~.,data=datos.training)
summary(modelo.lm)

#Coeficientes para Beta
modelo.lm$coefficients

prediccion <- predict(modelo.lm, datos.testing)
numero.predictoras <- dim(datos.testing)[2]

pre.lm <- indices.precision(datos.testing$ViolentCrimesPerPop, prediccion,numero.predictoras)
pre.lm





#==========================================================
#OPTATIVA
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 2/Datos_EjemplosClase_Tarea")
datos <- read.table('uscrime.csv',header=TRUE, sep=',',dec='.')


lm2 <- function(data) {
  x <- as.matrix(data[, -ncol(data)])
  y <- as.matrix(data[, ncol(data)])
  
  # Calcular los coeficientes de la regresión
  beta <- (solve( (t(x)%*%x) ) %*% (t(x) %*% y))
  
  # Crear un modelo con los coeficientes calculados
  model <- lm(y ~ x - 1, data = data.frame(cbind(x, y)))
  model$coefficients <- beta
  
  return(model)
}

predict2 <- function(modelo, datos_prueba) {
  predicciones <- predict(modelo, newdata = datos_prueba)
  return(predicciones)
}

modelo.lm<-lm(datos)
modelo.lm2<-lm2(datos)

summary(modelo.lm)
summary(modelo.lm2)

prediccion <- predict(modelo.lm, datos)
numero.predictoras <- dim(datos)[2]


pre.lm <- indices.precision(datos$ViolentCrimesPerPop, prediccion,numero.predictoras)
pre.lm

prediccion2 <- predict2(modelo.lm2, datos)
numero.predictoras <- dim(datos)[2]

pre.lm2 <- indices.precision(datos$ViolentCrimesPerPop, prediccion2,numero.predictoras)
pre.lm2

system.time(lm(datos), gcFirst = TRUE)
system.time(lm2(datos), gcFirst = TRUE)

system.time(predict(datos), gcFirst = TRUE)
system.time(predict2(datos), gcFirst = TRUE)





