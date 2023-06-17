library("FactoMineR") 
library("factoextra")
library("cluster")
library("fmsb")
library(tidyverse)
library(dendextend)
library(patchwork)

#================================================================================================
#==============================================================================================
#EJERCICIO #1

color <- c("red","blue")
color2 <- c("#FF6449", "#FEB035", "#FCE020", "#F7EC69", "#F1F8BE","#D5B9F6",
                     "#A2E3CD","#EDF380", "#D8FD9C", "#AEEC64", "#F598F8", "#9EFF37")
                     
color3 <- c("#FF6449", "#FEB035", "#FCE020", "#F7EC69", "#F1F8BE","#D5B9F6",
                     "#A2E3CD","#EDF380", "#D8FD9C", "#AEEC64", "#F598F8", "#9EFF37")
                     
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 4/DatosTarea")
Datos <- read.table("EjemploAlgoritmosRecomendacion.csv", header=TRUE, sep=';',dec=',',row.names=1)
#A

str(Datos)

#a) Ejecute el m´etodo k−medias con iter.max = 200, nstart = 100 para k = 4, luego
#desde RStudio verifique el Teorema de Fisher para este ejemplo

mis.datos <- scale(Datos)

grupos <- kmeans(x = mis.datos, centers = 4, iter.max = 200, nstart = 100)

# Verificación del Teorema de Fisher
grupos$totss==grupos$tot.withinss+grupos$betweenss     


#b) Ejecute el m´etodo k−medias con iter.max = 200, nstart = 100 , para esto encuentre
#valor de k usando los m´etodos Gap Statistic, wss y Average Silhouette usando la funci´on
#fviz nbclust, luego interprete los resultados usando interpretaci´on Horizontal-Vertical y
#gr´aficos tipo radar plot.


# Gap

fviz_nbclust(mis.datos, kmeans, method = "gap_stat", nstart = 100, iter.max = 200) +
  ggtitle("Gap Statistic")

#asociar a los emtodos de arriba
# Sacando Grupos
grupos<-kmeans(mis.datos,centers=1,iter.max=200,nstart=100)
# Centros
centros<-grupos$centers
# Gráfico Tipo Araña
rownames(centros)<-c("Cluster 1")
centros<-as.data.frame(centros)
maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)

radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color,plty=1,plwd=5,title="Comparación de clústeres")
legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color)
# Grafico de barras
barplot(t(grupos$centers),beside=TRUE,legend=colnames(datos),main = "Gráfico de Interpretación de Clases",col=color2,ylim=c(-1,4),cex.names = 0.7)



# WSS
fviz_nbclust(mis.datos, kmeans, method = "wss", nstart = 100, iter.max = 200) +
  ggtitle("Within-Cluster Sum of Squares")

#asociar a los emtodos de arriba
# Sacando Grupos
grupos<-kmeans(mis.datos,centers=2,iter.max=200,nstart=100)
# Centros
centros<-grupos$centers
# Gráfico Tipo Araña
rownames(centros)<-c("Cluster 1","Cluster 2")
centros<-as.data.frame(centros)
maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)

radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color,plty=1,plwd=5,title="Comparación de clústeres")
legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color)
# Grafico de barras
barplot(t(grupos$centers),beside=TRUE,main = "Gráfico de Interpretación de Clases",col=color2,ylim=c(-1,1),cex.names = 0.7)



# Silhouette Listo!
fviz_nbclust(mis.datos, kmeans, method = "silhouette", nstart = 100, iter.max = 200) +
  ggtitle("Average Silhouette")



# Sacando Grupos
grupos<-kmeans(mis.datos,centers=3,iter.max=200,nstart=100)
# Centros
centros<-grupos$centers
# Gráfico Tipo Araña
rownames(centros)<-c("Cluster 1","Cluster 2","Cluster 3")
centros<-as.data.frame(centros)
maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)

radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color2,plty=1,plwd=5,title="Comparación de clústeres")
legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2","Cluster 3"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color2)
# Grafico de barras
barplot(t(grupos$centers),beside=TRUE,legend=colnames(datos),main = "Gráfico de Interpretación de Clases",col=color2,ylim=c(-1.5,1.5),cex.names = 0.7)




#C

# Si se tienen 7 cl´usteres usando usando el m´etodo de k-medias ¿Qu´e productos recomendar´ıa
# a Teresa, a Leo y a Justin?, es decir, ¿los productos que compra cu´al otro cliente? Usando
# distancia eucl´ıdea ¿cu´al es la mejor recomendaci´on de compra que le podemos hacer a
# Teresa, a Leo y a Justin?

# Teresa y Marisol Cluster 1
# Leo y Susana Cluster 2
# Justin y Maura Cluster 6
grupos<-kmeans(mis.datos,centers=7,iter.max=200,nstart=100)
str(Datos)
datos.escalado <- data.frame(scale(Datos), grupos$cluster)
clusplot(datos.escalado, grupos$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)



#===================================================================================================
#===================================================================================================
#EJERCICIO #2

#a) Cargue la tabla de datos y ejecute un str(...), summary(...) y un dim(...), verifique
#la correcta lectura de los datos.

setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 5")
Datos <- read.csv('DatosBeijing.csv', header=TRUE, sep=',',dec='.',row.names=1)
str(Datos)
summary(Datos)
dim(Datos)

#b) Elimine las filas con NA usando el comando na.omit(...). ¿Cu´antas filas de eliminaron?
Datos.sin.na<-na.omit(Datos)
#Se eliminaron:
nrow(Datos)-nrow(Datos.sin.na)


#c) Elimine de la tabla de datos la variable DireccionViento. ¿Por que se debe eliminar?
Datos.sin.na <- subset(Datos.sin.na, select = -DireccionViento)
str(Datos.sin.na)
#se debe eliminar debido a que es de tipo cualitativa

#¿Que otra alternativa se tiene en lugar de eliminarla?
#convertirla a tipo factor por niveles, a tipo daisy


#d) ¿Qu´e pasa si ejecutamos un clustering jer´arquico con hclust(...). ¿Por qu´e sucede esto?
#modelo <- hclust(dist(Datos.sin.na),method = "complete")
#modelo


#e) Ejecute un k-medias con k = 3, iter.max=1000 y nstart=50.

grupos<-kmeans(Datos.sin.na,centers=3,iter.max=100, nstart=50)  
centros<-grupos$centers
rownames(centros)<-c("Cluster 1","Cluster 2","Cluster 3")
centros<-as.data.frame(centros)
maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)

color <- c("#FCEBB6","#78C0A8","#5E412F")

radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color,plty=1,plwd=5,title="Comparación de clústeres")

legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2","Cluster 3"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1,   horiz=FALSE,col=color)


#g) Construya el Codo de Jambu usando iter.max=100 y nstart=5,
#¿cu´antos conglomerados (cl´usteres) sugiere el codo? 
centers <- matrix(c(0,3,1,3,0,4), 3, 2, byrow=T)
generate <- function(n=50, extradim=0, sigma=1, mu=7) { 
  data1 <- matrix(rnorm(n*2), n, 2) * sigma 
  data1[,1] <- data1[,1] + centers[1,1] * mu
  data1[,2] <- data1[,2] + centers[1,2] * mu
  data2 <- matrix(rnorm(n*2), n, 2) * sigma 
  data2[,1] <- data2[,1] + centers[2,1] * mu
  data2[,2] <- data2[,2] + centers[2,2] * mu
  data3 <- matrix(rnorm(n*2), n, 2) * sigma 
  data3[,1] <- data3[,1] + centers[3,1] * mu
  data3[,2] <- data3[,2] + centers[3,2] * mu
  data <- rbind(data1,data2,data3)
  if (extradim > 0) {
    noise <- matrix(rnorm(3*n*extradim)*sigma, 3*n, extradim)
    data <- cbind(data, noise)
  }
  return(data)
}

Datos.sin.na <- generate(extradim=0)
InerciaIC<-rep(0,30)
for(k in 1:30) {
  grupos<-kmeans(Datos.sin.na,centers=k,iter.max = 100,nstart=5)
  InerciaIC[k]<-grupos$tot.withinss
}
plot(InerciaIC,col="blue",type="b")
mis.datos[1:10,]

#Utilice tambi´en el m´etodo silhouette de la funci´on
#fviz nbclust, ¿cu´antos conglomerados (cl´usteres) sugiere este m´etodo?
mis.datos <- scale(Datos.sin.na)
mis.datos
fviz_nbclust(mis.datos, kmeans, method = "silhouette",nstart = 100, iter.max = 5)
