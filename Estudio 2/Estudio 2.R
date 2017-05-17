install.packages('fpc')
install.packages('ggplot2')
install.packages('reshape2')
library(fpc)
library(ggplot2)
library(reshape2)
semilla<-7777777
set.seed(semilla) #seed del estudio
numero.revisiones=10000
#Seleccion de dataset a utilizar. En este estudio no se usan categorias o variables derivadas
data<-df.big.evol[sample(nrow(df.big.evol),numero.revisiones),!colnames(df.big.evol)%in%c()]


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


#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(data),c('rgroup'))
#Categoricas
catVars <- vars[sapply(data[,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(data[,vars],class) %in%  c('numeric','integer')]

#Escalo variables numericas
#Con esto busco depender menos de la escala de valores, SOLO NUMERICOS por utulizar clustering
pmatrix <- scale(data[,numericVars])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")


#Calculando cuantos K clusters usar
#Usando el criterio de indice de Calinski-Harabasz.
#(Promedio entre la varianza de clusters entre si y y el total de la varianza interna de los clusters.) )
system.time(clustering.ch <- kmeansruns(pmatrix, krange=1:20, criterion="ch",algorithm="MacQueen"))
clustering.ch$bestk #K recomendado por CH
#Usando el criterio de "Average silhouette width"
system.time(clustering.asw <- kmeansruns(pmatrix, krange=1:20, criterion="asw",algorithm="MacQueen"))#Criterio asw(average silhouette width)
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

kbest.p<-3#mas estables 12
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,count = TRUE, 
                   krange=kbest.p, seed=semilla,algorithm="MacQueen")
groups <- cboot$result$partition
#print_clusters(cboot$result$partition, kbest.p) Por si quiero ver el contenido
summary_clusters(cboot$result$partition, kbest.p)
cboot$bootmean 
cboot$bootbrd #veces disuelto
cboot$bootrecover #veces recuperado
