
#===================================================
#2 Introduzca x y y como vectores en R
x <- c(3, -5, 31, -1, -9, 10, 0, 18)
y <- c(1, 1, -3, 1, -99, -10, 10, -7)

#2.2 Calcule la media, la varianza, la raB4D1z cuadrada y la desviaciB4on estB4andar de y
print(mean(y))
print(var(y))
print(sqrt(y))
print(sd(y))

#2.3 Calcule la media, la varianza, la raB4D1z cuadrada y la desviaciB4on estB4andar de x
print(mean(x))
print(var(x))
print(sqrt(x))
print(sd(x))

#2.4
print(cor(x,y))

#==============================================================
#3

A<-matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=4,"byrow"="true")
print(A)

A[1,1:3]
A[1:4,2]
A[3,3] 
A[11] 
A[20]
A[5,4]
A[1,1,1] 



#================================================================
#5 

mi_df <- data.frame(
  "Peso" = c(76,67,55,57,87,48), 
  "Edad" = c(25,23,19,18,57,13), 
  "Nivel Educativo" = c("Lic","Bach","Bach","Bach","Dr","Msc")
)

print(mi_df)

#===============================================================


#==================================================================
#6

#6.1
x <- c(2, -5, 4, 6, -2, 8)
positivos<-x>0
y<-c(x[positivos])
print(y)

#6.2
v<-c(x[-1])
print(v)

#6.3
w<-c(x[i%%2==0])
print(w)





#===================================================
#7

x<-seq(0,2*pi,length=100)
plot(cos(x))
plot(x,cos(x),col="red")

#==================================================


#====================================================
#8
getwd()
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 1/DatosTarea1")
data <- read.csv("DJTable.csv", header = TRUE,
                                 sep = ";", dec = ",", row.names = 1)
plot(data[,"IBM"])


#====================================================
#9

#====================================================
#10
Datos <- read.table('EjemploAlgoritmosRecomendacion.csv',
                     header=TRUE, sep=';',dec=',',row.names=1)
Datos
dim(Datos)

class(Datos$VelocidadEntrega)

c(Datos[,1],Datos[,2])

summary(Datos)

str(Datos)

colMeans(Datos)


apply(Datos, 2, sd)

Datos <- read.table('EjemploAlgoritmosRecomendacion.csv',
                     header=TRUE, sep=';',dec='.',row.names=1)
dim(Datos)

class(Datos$VelocidadEntrega)

c(Datos[,1],Datos[,2])

summary(Datos)

str(Datos)

colMeans(Datos)

apply(Datos, 2, sd)



#============================================
#12
Datos <- read.table('SAheart.csv',header=TRUE, sep=';',dec=',')

dim(Datos)

Datos[1:3]

summary(Datos)

str(Datos)

#class(as.numeric(Datos$tobacco))

cor(as.numeric(Datos$tobacco),as.numeric(Datos$alcohol))

colSums(as.numeric(Datos))

sapply(Datos, is.numeric)
colSums(Datos[, numeric_cols])


#============================================
#13
ejercicio_13 <- function() {
  numeros <- sample(1:500, 200, replace = TRUE)
  
  cantidad <- sum(numeros >= 50 & numeros <= 450)
  
  return(cantidad)
}

# Llamamos a la funciC3n
ejercicio_13()



#============================================
#14 Traza

costo_llamada <- function(t) {
  if (t < 1) {
    costo <- 0.4
  } else {
    costo <- 0.4 + (t-1)/4
  }
  return(costo)
}

costo_llamada(2)


#============================================
#15 Traza
traza.matriz <- function(A) {
  n <- nrow(A)
  traza <- 0
  for (i in 1:n) {
    traza <- traza + A[i,i]
  }
  return(traza)
}
x<-matrix(data=c(9, 3, 4,1, 3, -1,4, 12, -2),nrow=3,ncol = 3, byrow = T)
x
traza.matriz(x)

#============================================
#16 Fibonacci
fibonacci <- function(n) {
  if (n <= 0) {
    return(NULL)
  } else if (n == 1) {
    return(0)
  } else if (n == 2) {
    return(c(0, 1))
  } else {
    fib <- c(0, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    return(fib)
  }
}
fibonacci(10)
#============================================
#17
mayor.cuadrado <- function(x) {
  i <- 0
  while (i^2 <= x) {
    i <- i + 1
  }
  return((i-1)^2)
}
mayor.cuadrado(30)


#============================================
#18

datos_alumnos <- data.frame(
  Nombre = c("Juan", "Pedro", "MarC-a", "Ana", "Luis", "Eva", "Diego", "Lucas", "Carmen", "Sara"), 
  Edad = c(20, 22, 21, 19, 23, 20, 24, 25, 20, 22),
  'AC1o de nacimiento' = 2023 - c(20, 22, 21, 19, 23, 20, 24, 25, 20, 22),
  'NC:mero de telC)fono' = c("555-1234", "555-5678", "555-9012", "555-3456", "555-7890", "555-2345", "555-6789", "555-0123", "555-4567", "555-8901"),
  row.names = c("Juan", "Pedro", "MarC-a", "Ana", "Luis", "Eva", "Diego", "Lucas", "Carmen", "Sara"))

# Mostramos el DataFrame creado
print(datos_alumnos)

#============================================
#19

#19.1
perm_rep <- function(n, r) {
  n^r
}
perm_rep(3,4)

#19.2
perm_sin_rep <- function(n, r) {
  factorial(n) / factorial(n-r)
}

#19.3
perm_rep_dist <- function(n, k) {
  factorial(n) / prod(factorial(rep(n/k, k)))
}

#19.4
comb <- function(n, r) {
  factorial(n) / (factorial(r) * factorial(n-r))
}

#19.5
comb_rep <- function(n, r) {
  comb(n+r-1, r)
}

#19.6
stirling_2 <- function(n, r) {
  sum((-1)^k * choose(r,k) * (r-k)^n, k=0:r)
}


#====================================================
#20
count_divisible_by_3 <- function(df) {
  # Creamos un vector lC3gico con TRUE en las posiciones que son divisibles por 3
  divisibles <- df %% 3 == 0
  
  # Contamos el nC:mero de valores TRUE en el vector
  count <- sum(divisibles)
  
  return(count)
}



#====================================================
#21
cal_cov_cor <- function(df, col1, col2) {
  var1 <- colnames(df)[col1]
  var2 <- colnames(df)[col2]
  covar <- cov(df[,col1], df[,col2])
  correl <- cor(df[,col1], df[,col2])
  return(list(variables = c(var1, var2), covarianza = covar, correlacion = correl))
}


#====================================================
#22
install.packages("readxl")
library(readxl)
Datos <- read_xlsx('EjemploAlgoritmosRecomendacion.xlsx')

summary(Datos)
str(Datos)

#====================================================
#23
U<-function(n){
  if(n==0) return(5)
  if(n==1) return(-5)
  if(n==2) return (2)
  else return(4*U(N-1) - 15*U(n-2)+U(n-3))
}
U(2)


#====================================================
#24
funcion <- function(x, n) {
  if (n == 0) {
    return(1)
  } else if (n == 1) {
    return(x)
  } else if (n > 1) {
    return(x * funcion(x, n-1))
  }
}

funcion(2,5)


#====================================================
#25

#install.packages("tidyr", dependencies = T)
#install.packages("dplyr" , dependencies = T)
install.packages("tidyverse" , dependencies = T)

#library(tidyr)
#library(dplyr)
library(tidyverse)

setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 1/DatosTarea1")
datos <- read.csv("bosques_energia.csv", header = TRUE,sep=',',dec='.')
datos

peq<- datos[c(1:9),c(1:7)]
peq
filt<-filter(
  datos, 
  Pais == "Canada" | Pais == "Paraguay" | Pais == "Paraguay" | Pais == "Peru" | Pais == "China",
  Indicador == "Consumo de energ??a renovable (% del consumo total de energ??a final)"
)
dataSet <- iconv(datos, 'UTF-8', 'ASCII')


print(datos %>% ggplot() + aes(x=Codigo.Pais, y=Pais))

filt


plot(filt)



pivot_wider(peq,names_from = c(c(X1960:X1963)),"Indicador",values_from = c(peq$Pais,peq$Codigo.Pais))

df_tidy<-peq %>% gather(Year, Value, X1960:X1976) %>%
  rename( Pais = "Pais", Codigo.Pais = "Codigo.Pais", Indicador = "Indicador")

df_tidy


peq
sin_na<-!is.na(peq)

tidy_datos[sin_na]
datos[tidy_datos]

tidy_datos <- datos %>% 
  pivot_longer(cols = X1960:X2006, names_to = "Anio", values_to = "Valor") %>% 
  select(-Codigo.Pais)



datos_limp <- tidy_datos %>% 
  select(Pais, Indicador, Valor,X2007:X2018,Anio) %>% 
  filter(!is.na(Valor))

datos_limp

pivot_wider(datos)




datos %>% filter(Pais %in% c("Aruba", "Angola")) %>%  head(5)

tidy_datos<-datos%>% gather()
tidy_datos
tidy_datos<-datos%>% gather()
tidy_datos

tidy_datos <- datos %>% gather(-c("X1960":"X1989"),c("X1995":"X"))
tidy_datos

sapply(datos, function(x) sum(is.na(x))>0)
datos

print(pivot_longer(datos,cols = c(Pais,"Codigo.Pais") , values_drop_na = F))

datos %>% 
  select(1:2)



#====================================================
#26
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 1/DatosTarea1")
datos <- read.csv("DatosEducacion.csv", header = TRUE,sep=',',dec='.')
datos %>% gather()

pivot_longer(datos,
             cols=c(indicador,"iso3"),
             values_drop_na = T)
datos_tidy<-
datos_tidy


