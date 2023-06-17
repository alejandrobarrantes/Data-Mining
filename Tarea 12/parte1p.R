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
    
    resultado <- clusterApply(clp, algoritmos, function(ptype) {
      MC <- ejecutar.prediccion(Datos, tipo ~ .,muestra, train.ada , type = ptype, iter=80, nu=1)
      si.val <- MC[2, 2]
      valores <- list(Tipo = ptype, Resultado = si.val)
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

# GRAFICACION
resultados <- data.frame("discrete"     = deteccion.si.discrete,
                         "real"     = deteccion.si.real,
                         "gentle" = deteccion.si.gentle) # Preparamos los datos

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Detección de los 1's detectados", 
        xlab = "Número de iteración",
        ylab = "Cantidad de 1's detectados",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda