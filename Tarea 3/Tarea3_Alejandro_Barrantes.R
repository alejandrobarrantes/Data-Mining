
library("FactoMineR")
library("factoextra")
library(ellipse)
library(tidyverse)
library(corrplot)
library(gpairs)
library(FactoMineR)
library(car)
library(GGally)
library(dplyr)

#EJERCICIO 1

setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 3/Datos de la Presentación y para la Tarea")
datos <- read.table('SpotifyTop2018_40_V2.csv',header=TRUE, sep=',',dec='.')
#datos


modelo <- prcomp(datos,scale. = TRUE,center = TRUE)
res<-PCA(datos, scale.unit=TRUE, ncp=5, graph = FALSE)
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))


#------------
# a)
summary(datos)

#------------
# b)
atipicos<-Boxplot(datos[,c(1,2)],data=datos[,c(1,2)],id.method="y",col="Blue")
atipicos
#------------
# c)

#------------
# d) Calcule la matriz de correlaciones, incluya alguna de las im´agenes que ofrece R e interpr´ete dos de las correlaciones. Debe ser una interpretaci´on dirigida a una persona que no sabe nada de estad´ıstica matriz.correlacion<-cor(datos2) corrplot(matriz.correlacion)
matriz.correlacion<-cor(datos)
corrplot(matriz.correlacion)

#En el grafico anterior se puede apreciar la correlacion entre energy y loudness, 

#---------
# e)

#En el cırculo de correlacion determine la correlacion entre las variables.
fviz_pca_var(res,col.var="steelblue",ggtheme = mi.tema,select.var = list(cos2 = 0.05))

#Explique la formacion de los clusteres basado en la sobre-posicion del cırculo y el plano
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.ind = list(cos2 = 0.05),select.var = list(cos2 = 0.05))


#En el plano de los componentes 1 y 3 interprete las canciones In My Feelings, In My
#Mind, Havana, Candy Paint y HUMBLE, que son mal representadas en los componentes 1 y 2.
fviz_pca_ind(res, pointsize = 5, pointshape = 21, fill = "#E7B800", repel = TRUE, select.ind = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 3))




#EJERCICIO #2

datos <- read.table('TablaAffairs.csv',header=TRUE, sep=';',dec='.')
#datos
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))



# a) Calcule el resumen numerico, interprete los resultados para una variable
datos2 <- datos[, sapply(datos, is.numeric)]
summary(datos2)
#str(datos)

#b) Calcule la matriz de correlaciones, incluya alguna de las imagenes que ofrece R e interprete dos de las correlaciones.
#Debe ser una interpretacion dirigida a una persona que no sabe nada de estadıstica.

matriz.correlacion<-cor(datos2[,-c(1)])
corrplot(matriz.correlacion)


# c) Usando solo las variables numericas efectue un ACP y de una interpretacion siguiendo los
#siguientes pasos: 1) Elimine de los graficos individuos y variables con menos del 5 % de
#calidad de representacion 2) en el plano principal encuentre 4 clusteres, 3) en el cırculo de
#correlacion determine la correlacion entre las variables y 4) explique la formacion de los
#clusteres basado en la sobre-posicion del cırculo y el plano.


modelo <- prcomp(datos2,scale. = TRUE,center = TRUE)
res<-PCA(datos2[,-c(1)], scale.unit=TRUE, ncp=5, graph = FALSE)

#en el plano principal encuentre 4 clusteres,
fviz_pca_ind(res,col.ind ="steelblue",ggtheme = mi.tema,select.ind = list(cos2 = 0.05))
#fviz_pca_var(res,col.var ="steelblue",ggtheme = mi.tema,select.var = list(cos2 = 0.05))


# 3) en el c´ırculo de correlaci´on determine la correlacion entre las variables 
fviz_pca_var(res,col.var="steelblue",ggtheme = mi.tema,select.var = list(cos2 = 0.05))

#y 4) Explique la formacion de los clusteres basado en la sobre-posicion del cırculo y el plano
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.ind = list(cos2 = 0.05),select.var = list(cos2 = 0.05))



#d) Ahora convierta las variables Genero e Hijos en Codigo Disyuntivo Completo y repita el ACP
#¿Se gana interpretabilidad al convetir Genero e Hijos en Codigo Disyuntivo Completo?

datos <- datos %>%dplyr::mutate(Genero = dplyr::recode(Genero,"male" = 1,"female" = 0))

datos <- datos %>% dplyr::mutate(Hijos = dplyr::recode(Hijos,"yes" = 1, "no" = 0))

datos <- datos %>% dplyr::mutate(Valoracion = dplyr::recode(Valoracion,
                                                            "muy infeliz" = 0,
                                                            "infeliz" = 1,
                                                            "neural"=2,
                                                            "feliz"=3,
                                                            "muy feliz"=4))

resDis<-PCA(datos[,-c(1,10)], scale.unit=TRUE, ncp=5, graph = FALSE)
fviz_pca_biplot(resDis,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.var = list(cos2 = 0.05), axes=c(1, 2))



# EJERCICIO 3

datos <- read.table('SAheart.csv',header=TRUE, sep=';',dec='.')
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))
datos
str(datos)


df_num <- df[, sapply(df, is.numeric)]

#a) Efect´ue un ACP usando solo las variables num´ericas y d´e una interpretaci´on siguiendo los 
#siguientes pasos (para todos los ejercicios elimine de los gr´aficos individuos y variables con                 
#menos del 5 % de calidad de representaci´on):

datos2 <- datos[, sapply(datos, is.numeric)]
summary(datos2)
res<-PCA(datos2, scale.unit=TRUE, ncp=5, graph = FALSE)


#En el plano principal encuentre los clusteres.
fviz_pca_ind(res,col.ind ="steelblue",ggtheme = mi.tema,select.ind = list(cos2 = 0.05))

#En el cırculo de correlacion determine la correlacion entre las variables.
fviz_pca_var(res,col.var="steelblue", select.var = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))


#Explique la formacion de los clusteres basado en la sobre-posicion del cırculo y el plano
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.ind = list(cos2 = 0.05),select.var = list(cos2 = 0.05))



#b) Efect´ue un ACP usando las variables num´ericas y las variables categ´oricas (recuerde recodificar las categ´oricas usando c´odigo disyuntivo completo). Luego d´e una interpretaci´on
#siguiendo los siguientes pasos:
datos <- datos %>%
  dplyr::mutate(famhist = dplyr::recode(famhist,
                                        "Present" = 1,
                                        "Absent" = 0))
str(datos)
datos <- datos %>%
  dplyr::mutate(chd = dplyr::recode(chd,
                                    "Si" = 1,
                                    "No" = 0))

res<-PCA(datos, scale.unit=TRUE, ncp=5, graph = FALSE)

#En el plano principal encuentre los clusteres.
fviz_pca_ind(res,col.ind ="steelblue",ggtheme = mi.tema,select.ind = list(cos2 = 0.05))

#En el cırculo de correlacion determine la correlacion entre las variables.
fviz_pca_var(res,col.var="steelblue", select.var = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))


#Explique la formacion de los clusteres basado en la sobre-posicion del cırculo y el plano
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.ind = list(cos2 = 0.05),select.var = list(cos2 = 0.05))
