library("FactoMineR") 
library("factoextra")
library("cluster")
library("fmsb")


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
modelo <- hclust(dist(Datos.sin.na),method = "complete")
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
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color)


#g) Construya el Codo de Jambu usando iter.max=100 y nstart=5,
#¿cu´antos conglomerados (cl´usteres) sugiere el codo? 
library(scatterplot3d)
library(mgcv)


Datos.sin.na <- generate(extradim=0)
InerciaIC<-rep(0,30)
for(k in 1:30) {
  grupos<-kmeans(Datos.sin.na,centers=k,iter.max = 100,nstart=5)
  InerciaIC[k]<-grupos$tot.withinss
}
plot(InerciaIC,col="blue",type="b")

#Utilice tambi´en el m´etodo silhouette de la funci´on
#fviz nbclust, ¿cu´antos conglomerados (cl´usteres) sugiere este m´etodo?

str(Datos.sin.na)
mis.datos <- scale((Datos.sin.na))
fviz_nbclust(Datos.sin.na, kmeans, method = "silhouette")
