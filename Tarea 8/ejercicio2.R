library(traineR)
library(caret)
library(rpart.plot)


#1. Cargue la tabla de datos titanicV2020.csv, aseg´urese re-codificar las variables cualitativas y
#de ignorar variables que no se deben usar.

setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 7/Datos Clase y Tareas")
datos <- read.table("titanicV2020.csv", header=TRUE, sep=',',dec='.',stringsAsFactors = T)

# Eliminamos las columnas PassengerId,Name,SibSp,Parch,Ticket, Cabin, Embarked: 
datos <- subset(datos, select = -c(PassengerId,Name,SibSp,Parch,Ticket, Cabin, Embarked))
#omitimos nulos
datos<-na.omit(datos)
#recodificamos
enteros <- sapply(datos, is.integer)
datos[enteros] <- lapply(datos[enteros], as.factor)


#2. Usando el comando sample de R genere al azar una tabla aprendizaje con un 80 % de los
#datos y con el resto de los datos genere una tabla de aprendizaje.

tam <- dim(datos)
n   <- tam[1]
muestra      <- sample(1:n, floor(n*0.20))
ttesting     <- datos[muestra,]
taprendizaje <- datos[-muestra,]


#3. Usando ´arboles de Decisi´on (con traineR) genere un modelo predictivo para la tabla
#de aprendizaje. Modifique los par´ametros del ´arbol de decisi´on para lograr los mejores
#resultados posibles. Grafique el ´arbol obtenido

tabla.comparacion <- data.frame()

modelo       <- train.rpart(Survived~.,data = taprendizaje, minsplit=2)
prediccion   <- predict(modelo, ttesting, type = 'class')
mc           <- confusion.matrix(newdata = ttesting, prediccion)
# Índices de Calidad de la predicción
general.indexes(mc = mc)

#Grafico del arbol
prp(modelo,extra=104,
    branch.type=2, 
    box.col=c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval])


# Con selección de variables
modelo <- train.rpart(formula = Survived~Pclass + Sex,taprendizaje,minsplit = 2)
prp(modelo,extra=104,
    branch.type=2, 
    box.col=c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval])


metricas<- c((general.indexes(mc=mc)[c(-1,-4)]),(unlist(general.indexes(mc=mc)[4])))
metricas$modelo <-"rpart"
tabla.comparacion <- rbind(tabla.comparacion,metricas)


#4. Construya una tabla para los ´ındices anteriores que permita comparar el resultado de los
#´arboles de Decisi´on con respecto a los m´etodos generados en las tareas anteriores ¿Cu´al
#m´etodo es mejor?
  

indices.comparacion <- function(kernels) {
  
  for (k in kernels) {
    if(k=="linear" || k=="radial" || k=="polynomial" || k =="sigmoid"){
      m <- train.svm(Survived~., taprendizaje, kernel = k)
      p <- predict(m, ttesting, type = "class")
      MC <- confusion.matrix(ttesting, p)
      m <- c((general.indexes(mc=MC)[c(-1,-4)]),(unlist(general.indexes(mc=MC)[4])))
      m$modelo <-k
      tabla.comparacion <- rbind(tabla.comparacion,m)
    }else{
      m <- train.knn(Survived~., taprendizaje, kernel = k)
      p <- predict(m, ttesting, type = "class")
      MC <- confusion.matrix(ttesting, p)
      m <- c((general.indexes(mc=MC)[c(-1,-4)]),(unlist(general.indexes(mc=MC)[4])))
      m$modelo <-k
      tabla.comparacion <- rbind(tabla.comparacion,m)
    }
    
  }
  
  return(tabla.comparacion)
}


kernels <- c("linear", "radial", "polynomial","sigmoid","rectangular","triangular",
             "epanechnikov","biweight","triweight","cos","inv","gaussian","optimal")


indices <- indices.comparacion(kernels)

indices <- select(indices, modelo, everything())
indices

