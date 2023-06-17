# ========================
#         Ejercicio 4
# ========================
install.packages("traineR")
library(traineR)



setwd("/Users/AndresR/Documents/Tarea6")
datos <- read.csv("titanicV2020.csv", header=TRUE, sep=",")
datos


str(datos)


datos$Survived <- factor(datos$Survived, levels = c(1,0))
datos$Pclass <- factor(datos$Pclass, levels = c(1, 2, 3))
datos$Sex <- factor(datos$Sex, levels = c("female", "male"))
datos$Embarked <- factor(datos$Embarked, levels = c("C", "Q", "S"))


# datos
#"PassengerId", "Ticket" y "Cabin" se pueden ignorar tambien
#se ignora la variable "Name"


datos <- datos[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]

datos
# Resumen numérico
summary(datos)

# Boxplot de las variables numéricas
boxplot(datos[,c("Age","SibSp","Parch","Fare")], main="Boxplot de Variables Numéricas")

# Correlación entre variables
cor(datos[,c("Age","SibSp","Parch","Fare")])

# Gráfico de barras de la variable categórica Pclass
library(ggplot2)
ggplot(datos, aes(x=Pclass, fill=Survived)) +
  geom_bar(position="dodge")

# Gráfico de barras de la variable categórica Sex
ggplot(datos, aes(x=Sex, fill=Survived)) +
  geom_bar(position="dodge")

# Gráfico de barras de la variable categórica Embarked
ggplot(datos, aes(x=Embarked, fill=Survived)) +
  geom_bar(position="dodge")


table(datos$Survived)
print("La salida  muestra que hay 815  pasajeros que no sobrevivieron y 494 pasajeros que sobrevivieron.")


print("Se puede concluir que este problema de predicción está desequilibrado, ya que la proporción de pasajeros que sobrevivieron es significativamente menor que la proporción de pasajeros que no sobrevivieron. Esto Puede afectar a la prediccion de la parte minoritaria.")


# Ejercicio 4.4


muestra80 <- sample(nrow(datos), 0.8 * nrow(datos))

taprendizaje <- datos[muestra80,]
taprendizaje
ttesting <- datos[-muestra80,]
ttesting


taprendizaje <- na.omit(taprendizaje)
ttesting <- na.omit(ttesting)


modelo<-train.knn(Survived~.,data=taprendizaje)
modelo

prediccion <- predict(modelo, ttesting, type = "class")
head(prediccion$prediction)
length(prediccion)

MC <- confusion.matrix(ttesting, prediccion)
MC
# Índices de Calidad de la predicción
general.indexes(mc = MC)

print("Global")
Global <- sum(diag(MC)) / sum(MC)
Global
print("categoría")
individual <- diag(MC) / rowSums(MC)



print(MC)
print(paste("Precisión global:", round(Global, 2)))
print(paste("Precisión categoría 0 (No sobrevivio):", round(individual[1], 2)))
print(paste("Precisión categoría 1 (Sobrevivio):", round(individual[2], 2)))


print(" La precisión global del modelo es de 0.8317, esto indica que el modelo clasifica de manera corecta al 83 porciento de los pasajeros aproximadamente. A su vez es de 0.73 para los pasajeros que nos sobrevivieron,mientras que para los que sí es de 0.9. Lo que indica que el modelo predice correctamente al 90 porciento de los que sí sobrevivieron")
print("Tal como podemos observar el modelo es bastante aceptable,aunque se podrían probar algunos otros valores con el fin de intentar mejorarlo")


# Ejercicio 4.5

datos2 <- datos[,c("Survived", "Pclass", "Sex", "Age", "Embarked")]



taprendizaje <- datos2[muestra80,]
taprendizaje
ttesting <- datos2[-muestra80,]
ttesting


taprendizaje <- na.omit(taprendizaje)
ttesting <- na.omit(ttesting)


modelo<-train.knn(Survived~.,data=taprendizaje)
modelo

prediccion <- predict(modelo, ttesting, type = "class")
head(prediccion$prediction)
length(prediccion)

MC <- confusion.matrix(ttesting, prediccion)
MC
# Índices de Calidad de la predicción
general.indexes(mc = MC)

print("Global")
Global <- sum(diag(MC)) / sum(MC)
Global
print("categoría")
individual <- diag(MC) / rowSums(MC)



print(MC)
print(paste("Precisión global:", round(Global, 2)))
print(paste("Precisión categoría 0 (No sobrevivio):", round(individual[1], 2)))
print(paste("Precisión categoría 1 (Sobrevivio):", round(individual[2], 2)))



print("Se puede observar que los valores de la precision global aumento en cierto punto,no de gran manera pero si es evidente un cambio en este y en los pasajeros que no sobrevivieron,cuyo valor de prediccion tambien aumentó")
