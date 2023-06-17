###Funciones y Librerias

library("FactoMineR") 
library("factoextra")
library("factoextra")
library("corrplot")
library("car")
library("dplyr")

###


df <- read.csv("C:\\Users\\jjgr2\\OneDrive\\Escritorio\\Semestre I\\Mineria de Datos\\Tarea3\\Datos de la Presentación y para la Tarea\\SAheart.csv"
               , header = TRUE, dec=",", sep=";")
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))

df$tobacco <- as.numeric(df$tobacco)
df$ldl <- as.numeric(df$ldl)
df$adiposity <- as.numeric(df$adiposity)
df$obesity <- as.numeric(df$obesity)
df$alcohol <- as.numeric(df$alcohol)


df_num <- df[, sapply(df, is.numeric)]

#a

res<-PCA(df_num, scale.unit=TRUE, ncp=5, graph = FALSE)

# 1
#Individuos
fviz_pca_ind(res, pointsize = 1, pointshape = 1, fill = "#E7B800", repel = TRUE, select.ind = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

##
## IMAGEN
##


# 2
#Variables
fviz_pca_var(res,col.var="steelblue", select.var = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

# 3
#Biplot
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.var = list(cos2 = 0.05), axes=c(1, 2))

##
## IMAGEN
##



#b

df <- df %>%
  dplyr::mutate(famhist = dplyr::recode(famhist,
                                              "Present" = 1,
                                              "Absent" = 0))
df <- df %>%
  dplyr::mutate(chd = dplyr::recode(chd,
                                        "Si" = 1,
                                        "No" = 0))

res<-PCA(df_num, scale.unit=TRUE, ncp=5, graph = FALSE)

# 1
#Individuos
fviz_pca_ind(res, pointsize = 1, pointshape = 1, fill = "#E7B800", repel = TRUE, select.ind = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

##
## IMAGEN
##


# 2
#Variables
fviz_pca_var(res,col.var="steelblue", select.var = list(cos2 = 0.05),ggtheme = mi.tema, axes=c(1, 2))

# 3
#Biplot
fviz_pca_biplot(res,col.var = "#2E9FDF",col.ind = "#696969",ggtheme = mi.tema, select.var = list(cos2 = 0.05), axes=c(1, 2))

##
## IMAGEN
##













