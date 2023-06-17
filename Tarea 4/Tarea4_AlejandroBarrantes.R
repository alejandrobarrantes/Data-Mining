library("FactoMineR") 
library("factoextra")
library("cluster")
library("fmsb")

# Valores de los gráficos por defecto
mi.tema <- theme_grey() + theme(panel.border = element_rect(fill = NA,color = "white"), plot.title = element_text(hjust = 0.5))

#EJERCICIO 1
setwd("~/ALEJANDRO/1er Ciclo 2023/Mineria de Datos/Tarea 4/DatosTarea")
Datos <- read.csv('EjemploAlgoritmosRecomendacion.csv', header=TRUE, sep=';',dec=',',row.names=1)
centroide <- function(num.cluster, datos, clusters) {
  ind <- (clusters == num.cluster)
  return(colMeans(datos[ind,]))
}

#a) Ejecute un Clustering Jerarquico con la distancia euclıdea 
#y la agregacion del Salto Maximo, 
#Salto Mınimo, Promedio y Ward. Guarde la tabla de datos en el archivo
#AlgoritmosRecomendacion2.csv con el cluster al que pertenece cada individuo para el
#caso de la agregacion de Ward usando 2 clusteres

# a.1) Ejecute un Clustering Jerarquico con la distancia euclıdea
Datos<-dist(Tabla, method = "euclidean")


#a.2) Salto Maximo
modelo <- hclust(dist(Datos),method = "complete")
plot(modelo)

#a.3) Salto Mınimo
modelo <- hclust(dist(Datos),method = "single")
plot(modelo)

#fviz_dend(modelo, k = 3, cex = 1.3, color_labels_by_k = FALSE, rect = TRUE,k_colors = c("#1B9E77", "#D95F02", "#7570B3"), ggtheme = mi.tema)

#a.4) Promedio
modelo <- hclust(dist(Datos),method = "average")
plot(modelo)

#rect.hclust(modelo, k=3, border="green")


# a.5) Ward
modelo <- hclust(dist(Datos),method= "ward.D")
plot(modelo)

#a.6) Guarde la tabla de datos en el archivo
#AlgoritmosRecomendacion2.csv con el cluster al
#que pertenece cada individuo para el
#caso de la agregacion de Ward usando 2 clusteres
Grupo<-cutree(modelo,k=2)
NDatos<-cbind(Datos,Grupo)
NDatos
write.csv(NDatos,"EjemploAlgoritmosRecomendacion2.csv")

#---------------------

#1.b) “Corte” el arbol anterior usando 2 clusteres y la agregacion 
#de Ward, interprete los resultados usando interpretacion usando 
#gr´aficos de barras (Horizontal-Vertical) y usando
#gr´aficos tipo Radar

modelo <- hclust(dist(Datos),method= "ward.D")
grupos <- cutree(modelo, k=2)

NDatos<-cbind(Datos,grupos)
NDatos

centro.cluster1<-centroide(1,(Datos),(grupos))
centro.cluster2<-centroide(2,(Datos),(grupos))
centros<-rbind(centro.cluster1,centro.cluster2)

maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)

color <- c("#ECD078","#D95B43","#C02942","#542437","#53777A")


radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color,plty=1,plwd=5,title="Comparación de clústeres")

legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color)

barplot(centros[1,],col=color,las=2, cex.names = 0.8, ylim = c(0,10))

barplot(centros[2,],col=color,las=2, cex.names = 0.8, ylim = c(0,10))

#En el grafico tipo radar podemos notar como hay un cluster que le dio bastante valoracion a todas las variables
# menos al tamannio del paquete, mientras que en el cluster 2, los clientes calificaron
# mas alto el tamannio del paquete.
# Mientras que en los graficos de barras podemos observar que ambos clusteres
# clasificaron de forma parecida, sin embargo, en el cluster #2 las calificaciones
# fueron mas bajas en todas las variables, solamente durabilidad y tamannio del paquete 
# se mantienen muy similares

#color <- c("#ECD078","#D95B43","#C02942","#542437","#53777A")
#barplot(NDatos,col=color,las=2, cex.names = 0.8, ylim = c(0,10))

#1.c) Si se tienen 4 cl´usteres usando agregaci´on de Ward ¿Qu´e productos recomendar´ıa a Teresa,
#a Leo y a Justin?, es decir, ¿los productos que compra cu´al otro cliente? Usando distancia
#eucl´ıdea ¿cu´al es la mejor recomendaci´on de compra que le podemos hacer a Teresa, a Leo
#y a Justin?

modelo <- hclust(dist(Datos),method= "ward.D")
grupos <- cutree(modelo, k=4)
grupos
NDatos<-cbind(Datos,grupos)


grupo.teresa<-NDatos["Teresa",]$grupos
grupo.leo<-NDatos["Leo",]$grupos
grupo.justin<-NDatos["Justin",]$grupos

#Recomendaciones para Teresa:
#Comprar los productos que compran
row.names(NDatos[NDatos$grupos==grupo.teresa,])

#Recomendaciones para Leo:
#Comprar los productos que compran
row.names(NDatos[NDatos$grupos==grupo.leo,])

#Recomendaciones para Justin:
#Comprar los productos que compran
row.names(NDatos[NDatos$grupos==grupo.justin,])


D<-dist(Datos, method = "euclidean")
as.matrix(D)[1:3,1:3]

#centro.cluster1<-centroide(1,as.matrix(Datos),as.matrix(grupos))
#centro.cluster1
#centro.cluster2<-centroide(2,as.matrix(Datos),as.matrix(grupos))
#centro.cluster2
#centro.cluster3<-centroide(3,as.matrix(Datos),as.matrix(grupos))
#centro.cluster3
#centro.cluster4<-centroide(4,as.matrix(Datos),as.matrix(grupos))
#centro.cluster4


#centros<-rbind(centro.cluster1,centro.cluster2,centro.cluster3,centro.cluster4)
#centros

#color <- c("#ECD078","#D95B43","#C02942","#542437","#53777A")
#barplot(t(centros),beside=TRUE,col=color,las=2, cex.names = 0.8, ylim = c(0,10))



#1.d) Construya un clustering jerarquico sobre las componentes principales del ACP.
Datos <- read.csv('EjemploAlgoritmosRecomendacion.csv', header=TRUE, sep=';',dec=',',row.names=1)
res  <- PCA(Datos , scale.unit=TRUE, ncp=5, graph = FALSE)
res.hcpc <- HCPC(res, nb.clust = -1, consol = TRUE, min = 2, max = 2, graph = FALSE)
#plot.HCPC(res.hcpc, choice="bar")

fviz_cluster(res.hcpc,repel = TRUE,show.clust.cent = TRUE,palette = "jco",main = "Factor map",geom = "text", select.ind = list(cos2 = 0.1))

#====================================================================================
#====================================================================================

#EJERCICIO 2

Datos <- read.csv("VotosCongresoUS.csv",header=TRUE, sep=",", dec=".")
str(Datos)
dim(Datos)
Datos<-as.data.frame(lapply((Datos), factor))
str(Datos)
dim(Datos)
#a) Ejecute una clasificaci´on jer´arquica sobre esta tabla de datos usando la funci´on daisy
#ya que los datos son cualitativos. Use m´etrica euclidean y m´etodo complete (deje el
#resultado en la variable jer). Cargue los datos con la siguiente instrucci´on:

D<-daisy(as.data.frame(Datos), metric = "euclidean")
D


jer<-hclust(D, method = "complete")
dim(as.matrix(jer))
jer
#b) Luego “corte” el ´arbol usando 3 cl´usteres y ejecute el siguiente c´odigo

grupo<-cutree(jer, k = 3)
NDatos<-cbind((Datos),grupo)
dim(NDatos)
cluster<-NDatos$grupo
sel.cluster1<-match(cluster,c(1),0)
Datos.Cluster1<-NDatos[sel.cluster1>0,]
dim(Datos.Cluster1)
sel.cluster2<-match(cluster,c(2),0)
Datos.Cluster2<-NDatos[sel.cluster2>0,]
dim(Datos.Cluster2)
sel.cluster3<-match(cluster,c(3),0)
Datos.Cluster3<-NDatos[sel.cluster3>0,]
dim(Datos.Cluster3)

str(Datos.Cluster1$Party)
Datos.Cluster1$Party

#Explique qu´e hace el codigo anterior. Luego ejecute el siguiente c´odigo:

plot(Datos$Party,col=c(4,6),las=2,main="Party",xlab="Todos los Datos")
plot(Datos.Cluster1$Party,col=c(4,6),las=2,main="Party",xlab="Cluster-1")
plot(Datos.Cluster2$Party,col=c(4,6),las=2,main="Party",xlab="Cluster-2")
plot(Datos.Cluster3$Party,col=c(4,6),las=2,main="party",xlab="Cluster-3")


#Con ayuda de los gr´aficos anteriores y tomando en cuenta el tama˜no de cada cluster
#interprete los 3 cl´usteres formados.

#================================================================
#=====================================================================

#EJERCICIO 3
#Realice un an´alisis similar al del ejercicio anterior con la tabla de datos
#CompraBicicletas.csv
Datos <- read.csv("CompraBicicletas.csv",header=TRUE, sep=";", dec=".")

dim(Datos)
str(Datos)

chr_vars <- sapply(Datos, is.character)
Datos[chr_vars] <- as.data.frame(lapply(Datos[chr_vars], factor))
str(Datos)


D<-daisy(as.data.frame(Datos), metric = "euclidean")
D

jer<-hclust(D, method = "complete")
jer

grupo<-cutree(jer, k = 3)
NDatos<-cbind((Datos),grupo)
dim(NDatos)
cluster<-NDatos$grupo
sel.cluster1<-match(cluster,c(1),0)
Datos.Cluster1<-NDatos[sel.cluster1>0,]
dim(Datos.Cluster1)
sel.cluster2<-match(cluster,c(2),0)
Datos.Cluster2<-NDatos[sel.cluster2>0,]
dim(Datos.Cluster2)
sel.cluster3<-match(cluster,c(3),0)
Datos.Cluster3<-NDatos[sel.cluster3>0,]
dim(Datos.Cluster3)

#Explique qu´e hace el c´odigo anterior. Luego ejecute el siguiente c´odigo:
color <- c("#ECD078","#D95B43","#C02942","#542437","#53777A")
plot(Datos$Education,col=color,las=2,main="Education",xlab="Todos los Datos")
plot(Datos.Cluster1$Education,col=c(4,6,7,8,9),las=2,main="Party",xlab="Cluster-1")
plot(Datos.Cluster2$Education,col=color,las=2,main="Party",xlab="Cluster-2")
plot(Datos.Cluster3$Education,col=color,las=2,main="party",xlab="Cluster-3")



#=================================================
#=====================================================

#EJERCICIO 4
matriz<- matrix(c(0,5,2,3,5,0,1,7,7,1,0,6,3,7,6,0), nrow = 4, ncol = 4)
matriz<-as.dist(matriz)

matriz

matriz.distancias<-dist(matriz, method = "euclidean")
matriz.distancias


mod<-hclust(matriz,method = "complete")
plot(mod)

mod<-hclust(matriz,method = "average")
plot(mod)



