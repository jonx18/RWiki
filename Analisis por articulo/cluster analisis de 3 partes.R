library(fpc)
library(ggplot2)
library(reshape2)
library(cluster)

#data<-df.articulos.evol[[33]]#arranco con el total de los datos
data<-df.pages#arranco con el total de los datos
#data<-df.pages.partes[,-partes]#arranco con el total de los datos
set.seed(7777777) #semilla para repeticion

#Funciones de imprecion
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(data[labels==i,numericVars])
  }
}
summary_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(summary(data[labels==i,]))
  }
}
table_summary_clusters <- function(labels, k,datos) {
  for(i in 1:k) {
    tmp<-summary(datos[labels==i,])
    tmp<-gsub("Min.   :", "", tmp)
    tmp<-gsub("1st Qu.:", "", tmp)
    tmp<-gsub("Median :", "", tmp)
    tmp<-gsub("Mean   :", "", tmp)
    tmp<-gsub("3rd Qu.:", "", tmp)
    tmp<-gsub("Max.   :", "", tmp)
    write.table(tmp, "C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\mydata.txt", sep="\t",append = TRUE,col.names = FALSE)
    print(tmp)
  }
}




#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(data),c('rgroup'))
#Categoricas
catVars <- vars[sapply(data[,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(data[,vars],class) %in%  c('numeric','integer')]

#Escalo variables numericas
#Con esto busco depender menos de la escala de valores, SOLO NUMERICOS (pierdo pagename)
pmatrix <- scale(data[,numericVars])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")


#Calculando cuantos K clusters usar
#Usando el criterio de indice de Calinski-Harabasz.
#(Promedio entre la varianza de clusters entre si y y el total de la varianza interna de los clusters.) )
clustering.ch <- kmeansruns(pmatrix, krange=1:20, criterion="ch")
clustering.ch$bestk #K recomendado por CH
#Usando el criterio de "Average silhouette width" (tarda mucho)
clustering.asw <- kmeansruns(pmatrix, krange=1:20, criterion="asw")#Criterio asw(average silhouette width)
clustering.asw$bestk #K recomendado por ASW
#Grafico para observar posibles K
critframe <- data.frame(k=1:20, ch=scale(clustering.ch$crit),
                        asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")

ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:20, labels=1:20)

#K seleccionados para probar.  2,4,5,10,13,14,15,18
#cboot$bootmean<0.6 es inestable
#0.6<cboot$bootmean<0.75 indica que se esta midiendo un patron en los datos
#por encima de 0.85 son estables
#cboot$bootbrd es la cantidad de veces que se disolvio un cluster.
#Basicamente si se disolvio mucho probablemente no sea un cluster real




kbest.p<-5#mas estables 5
print(kbest.p)
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,count = TRUE, 
                   krange=kbest.p, seed=7777777)
groups <- cboot$result$partition
#print_clusters(cboot$result$partition, kbest.p) Por si quiero ver el contenido
summary_clusters(cboot$result$partition, kbest.p)
table_summary_clusters(cboot$result$partition, kbest.p,data)
cboot$bootmean 
cboot$bootbrd #veces disuelto
cboot$bootrecover #veces recuperado
table(cboot$result$partition)#numero de elementos


#Visualizacion y analicis
clusplot(pmatrix, cboot$result$partition,color=TRUE, shade=TRUE, labels=2, lines=0)

#aplico kmeans
pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=100)
table(pclusters$cluster)#numero de elementos
#prueba asignando
sqr_edist <- function(x, y) {#distancia de cuadrados entre dos vectores
  sum((x-y)^2)
}
assign_cluster <- function(newpt, centers, xcenter=0, xscale=1) {
  xpt <- (newpt - xcenter)/xscale
  dists <- apply(centers, 1, FUN=function(c0){sqr_edist(c0, xpt)})
  which.min(dists)
}
tmp <- df.big.evol[,colnames(df.articulos.df.big.evol) %in% colnames(pmatrix)]
result <- data.frame()
for(i  in 1:dim(tmp)[1]){
  result[i,1] <- assign_cluster(tmp[i,],pclusters$centers,pcenter,pscale)
}
table(result)
table_summary_clusters(result, kbest.p,df.big.evol)
