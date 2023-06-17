###Funciones y Librerias

library("FactoMineR") 
library("factoextra")
library("factoextra")
library("corrplot")
library("car")
library("dplyr")

df <- read.csv("C:\\Users\\jjgr2\\OneDrive\\Escritorio\\Semestre I\\Mineria de Datos\\Tarea3\\Datos de la Presentación y para la Tarea\\TablaAffairs.csv"
               , header = TRUE, dec=",", sep=";")
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))
df$Edad <- as.numeric(df$Edad)
df$AnnosCasado <- as.numeric(df$AnnosCasado)
df$Religioso <- as.numeric(df$Religioso)
df$Educacion <- as.numeric(df$Educacion)

df <- subset(df, select = -c(X))
df_num <- df[, sapply(df, is.numeric)]
df_notNum <- df[, !sapply(df, is.numeric)]

#View(df_num)
#View(df_notNum)

# a
summary(df_num)

# b
cor(df_num)
corrplot(cor(df_num), method = "circle")

#Usando solo las variables num´ericas efect´ue un ACP y d´e una interpretaci´on siguiendo los
#siguientes pasos: 1) Elimine de los gr´aficos individuos y variables con menos del 5 % de
#calidad de representaci´on 2) en el plano principal encuentre 4 cl´usteres, 3) en el c´ırculo de
#correlaci´on determine la correlaci´on entre las variables y 4) explique la formaci´on de los
#cl´usteres basado en la sobre-posici´on del c´ırculo y el plano.


# c
#FactoExtra
res<-PCA(df_num, scale.unit=TRUE, ncp=5, graph = FALSE)



## 1) Elimine de los gr´aficos individuos y variables con menos del 5 % de calidad de representaci´on
#Individuos
fviz_pca_ind(res, pointsize = 5, pointshape = 21, fill = "#E7B800", repel = TRUE, select.ind = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

#Variables
fviz_pca_var(res,col.var="steelblue", select.var = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

#Biplot
# 2) en el plano principal encuentre 4 cl´usteres
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.var = list(cos2 = 0.05), axes=c(1, 2))
##
## IMAGEN
##


# 3
#Variables
fviz_pca_var(res,col.var="steelblue", select.var = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

# En el grafico de variables, podemos apreciar 3 cosas importantes.
# 1 - TiempoInfiel y Religioso constituyen la menor correlacion con
# con la componente principal, y en este caso TiempoInfiel es la menos
# relevante, por otro lado esta dos mismas,no tiene una correlacion significativa
# con Educacion y Ocupacion esto dado por su angulo, pero si tienen una
# fuerte correlacion con Edad y AnnosCasado.

# 2 - El segundo importante es que tanto Edad como AnnosCasado son dos
# de las variables mas importantes para la principal componente por el 
# largo de de las flechas, y muestran tambien una correlacion insignificante
# con Educacion y Ocupacion, tambien muestran correlacion significativa y positiva con
# TiempoInfiel y Religioso, lo que indica que en medida de que alguna de estas 4
# variables se desplace las otras 3 tambien lo haran.

# 3- Educacion y ocupacion tienen un correlacion significativa y positiva entre si, 
# una alta correlacion con la principal componente y una correlacion insignificante
# con las demas variables.



# 4
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.var = list(cos2 = 0.05), axes=c(1, 2))

##
## IMAGEN
##
# Para explicar la formacion del plano y el circulo usaremos la 
# imagen del biplot de donde extraimos los cluster
# Y se puede ver como hay un monton de datos insignificantes, 
# y probablemente esto se deba al recorte de variables que realizamos
#  por lado se realizaron los clusters en base a las variables
# que si tenemos y se realizo una asociacion para esta varaibles, 
# sin embargo, tal vez no hayan sido la mejor eleccion, y se deba
# revisar mas al fondo este analisis, junto con las otras variables
# y el plano 1-3. 



# d
#Ahora convierta las variables G´enero e Hijos en C´odigo Disyuntivo Completo y repita el
#ACP ¿Se gana interpretabilidad al convetir G´enero e Hijos en C´odigo Disyuntivo Completo?

df <- df %>%
  dplyr::mutate(Genero = dplyr::recode(Genero,
                                       "male" = 1,
                                       "female" = 0))
df <- df %>%
  dplyr::mutate(Hijos = dplyr::recode(Hijos,
                                       "yes" = 1,
                                       "no" = 0))
df_numD <- df[, sapply(df, is.numeric)]
resD<-PCA(df_numD, scale.unit=TRUE, ncp=5, graph = FALSE)
fviz_pca_biplot(resD,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.var = list(cos2 = 0.05), axes=c(1, 2))

# Aca se pregunta primero que todo si "¿Se gana interpretabilidad al convetir Genero e Hijos en Codigo Disyuntivo Completo?"
# Y Podriamos decir que si, al tener la posibilidad de dividir los datos un poco mejor y realizar
# cluster mas precisos, por otro lado tambien podemos apreciar que perdimos una varaible (TiempoInfiel)
# ya habiamos mencionado anteriormente que no era una variable significativa, y al tener un esquema
# nuevo de analisis nos aseguramos que es una variable innecesaria. Por otro lado tenemos seguimos teniendo
# un monton de datos en el Cuadrante 2 y 3 que siguen desasociado a una variable lo que sugiere que 
# son datos insignificantes y para hacer un analisis precisos considero que deberian sacarse del esquema.
