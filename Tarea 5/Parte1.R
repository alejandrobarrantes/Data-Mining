detach("package:tidyverse", unload = TRUE)
detach("package:dendextend", unload = TRUE)
detach("package:factoextra", unload = TRUE)
detach("package:patchwork", unload = TRUE)
detach("package:fmsb", unload = TRUE)
detach("package:cluster", unload = TRUE)


library(tidyverse)
library(dendextend)
library(factoextra)
library(patchwork)
library(fmsb)
library(cluster)


color <- c("red","blue")
color2 <- c("#FF6449", "#FEB035", "#FCE020", "#F7EC69", "#F1F8BE","#D5B9F6",
            "#A2E3CD","#EDF380", "#D8FD9C", "#AEEC64", "#F598F8", "#9EFF37")

color3 <- c("#FF6449", "#FEB035", "#FCE020", "#F7EC69", "#F1F8BE","#D5B9F6",
            "#A2E3CD","#EDF380", "#D8FD9C", "#AEEC64", "#F598F8", "#9EFF37")


datos <- read.table("C:\\Users\\jjgr2\\OneDrive\\Escritorio\\Semestre I\\Mineria de Datos\\Tarea4\\EjemploAlgoritmosRecomendacion.csv", header=TRUE, sep=';',dec=',',row.names=1)
#A


mis.datos <- scale(datos)

k4 <- kmeans(x = mis.datos, centers = 4, iter.max = 200, nstart = 100)

summary(k4)

# Verificación del Teorema de Fisher
k4$totss==k4$tot.withinss+k4$betweenss     



#B

# Gap

fviz_nbclust(mis.datos, kmeans, method = "gap_stat", nboot = 50, nstart = 100, iter.max = 200) +
  ggtitle("Gap Statistic")

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
barplot(t(grupos$centers),beside=TRUE,legend=colnames(datos),main = "Gráfico de Interpretación de Clases",col=color2,ylim=c(-1,1),cex.names = 0.7)



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
barplot(t(grupos$centers),beside=TRUE,legend=colnames(datos),main = "Gráfico de Interpretación de Clases",col=color2,ylim=c(-1,1),cex.names = 0.7)



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
datos.escalado <- data.frame(scale(datos), grupos$cluster)
clusplot(datos.escalado, grupos$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)





