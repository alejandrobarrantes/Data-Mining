# EJERCICIO #5
library("traineR")
getwd()

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