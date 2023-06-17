Clase <- c(1,0,0,1,0,0,1,0,0,1)
Score <- c(0.61, 0.06,0.80, 0.11, 0.66, 0.46, 0.40, 0.19, 0.00, 0.91)


# Graficamos ROC con funciones de paquete ROCR
plotROC(Score, Clase)

# Graficamos puntos con algoritmo
i <- 1  # Contador
FP_r <- -1  # Para que entre al condicional en la primera iteración
TP_r <- -1  # Para que entre al condicional en la primera iteración

for(Umbral in seq(1, 0, by = -0.05)) {
  
  Prediccion <- ifelse(Score >= Umbral, 1, 0)
  
  MC <- table(Clase, Pred = factor(Prediccion, levels = c(0, 1)))
  
  # Condicional para no imprimir puntos repetidos
  # if(FP_r != MC[1, 2] / sum(MC[1, ]) | TP_r != MC[2, 2] / sum(MC[2, ])) {
    
    FP_r <- MC[1, 2] / sum(MC[1, ])  # Tasa de Falsos Positivos
    TP_r <- MC[2, 2] / sum(MC[2, ])  # Tasa de Verdaderos Positivos
    
    # Graficamos punto
    points(FP_r, TP_r, col = "blue")
    text(FP_r + 0.02, TP_r - 0.02, Umbral)
    
    # Imprimimos resultados
    cat("Punto i = ", i, "\n")  
    cat("Umbral = T = ", Umbral, "\n")
    cat("MC = \n")
    print(MC)
    cat("Tasa FP = ", round(FP_r, 2), "\n")
    cat("Tasa TP = ", round(TP_r, 2), "\n") 
    cat("\n") 
    
    i <- i + 1  # Aumentamos contador
    
  # }
  
}


Clase <- c(1,0,0,1,0,1,0,1,0,0)
Score <- c(0.91,0.80, 0.66, 0.61, 0.46, 0.40, 0.19, 0.11, 0.06, 0.00)

Umbral<-0.5
Paso <- 0.1

N <- 6 # ceros
P <- 4 # unos

TP <- 0 
FP <- 0

for(i in 1:10) { 
  
  if(Score[i] > Umbral)
    if(Clase[i] == 1)
      TP <- TP + 1
  else 
    FP <- FP + 1
  else 
    if(Clase[i] == 0)
      FP <- FP + 1
    else 
      TP <- TP + 1
    
    # Graficamos punto
    points(FP / N, TP / P, col = "blue")
    text(FP / N + 0.02, TP / P - 0.02, i)
    
    Umbral <- Umbral + Paso  
  
    cat("\n")
    print("=======================")
    print(paste("Punto: ",i ))
    print(paste(Umbral,"> T y class=",Clase[i],ifelse(Clase[i]==1,
                                                      paste("=> TP=", TP),paste("=> FP=", FP))))
    print(paste("FP=", FP))
    print(paste("FP=", TP))
    print(paste("FP/10=", FP/10))
    print(paste("TP/10=", TP/10))
    print(paste("Punto: (", FP / N ,",", TP / P,")"))
    
}






#====================================


library(ROCR)

Clase <- c(1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1)
Score <- c(0.6, 0.35, 0.53, 0.54, 0.505, 0.3, 0.8, 0.1, 0.33, 0.39, 0.34, 0.55, 0.51, 0.38, 0.36, 0.52, 0.9, 0.7, 0.37, 0.4)

# Graficamos ROC con funciones de paquete ROCR
plotROC(Score, Clase)

# Graficamos puntos con algoritmo
i <- 1  # Contador
FP_r <- -1  # Para que entre al condicional en la primera iteración
TP_r <- -1  # Para que entre al condicional en la primera iteración

for(Umbral in seq(1, 0, by = -0.005)) {
  
  Prediccion <- ifelse(Score >= Umbral, 1, 0)
  
  MC <- table(Clase, Pred = factor(Prediccion, levels = c(0, 1)))
  
  # Condicional para no imprimir puntos repetidos
  if(FP_r != MC[1, 2] / sum(MC[1, ]) | TP_r != MC[2, 2] / sum(MC[2, ])) {
    
    FP_r <- MC[1, 2] / sum(MC[1, ])  # Tasa de Falsos Positivos
    TP_r <- MC[2, 2] / sum(MC[2, ])  # Tasa de Verdaderos Positivos
    
    # Graficamos punto
    points(FP_r, TP_r, col = "blue")
    text(FP_r + 0.02, TP_r - 0.02, Umbral)
    
    # Imprimimos resultados
    cat("Punto i = ", i, "\n")  
    cat("Umbral = T = ", Umbral, "\n")
    cat("MC = \n")
    print(MC)
    cat("Tasa FP = ", round(FP_r, 2), "\n")
    cat("Tasa TP = ", round(TP_r, 2), "\n") 
    cat("\n") 
    
    i <- i + 1  # Aumentamos contador
    
  }
  
}
