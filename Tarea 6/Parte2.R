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


