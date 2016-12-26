library(fpc)
library(ggplot2)
library(reshape2)

data<-df.articulos$julio #arranco con el total de los datos
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

#K seleccionados para probar. 10 , 11 , 12 , 13
#cboot$bootmean<0.6 es inestable
#0.6<cboot$bootmean<0.75 indica que se esta midiendo un patron en los datos
#por encima de 0.85 son estables
#cboot$bootbrd es la cantidad de veces que se disolvio un cluster.
#Basicamente si se disolvio mucho probablemente no sea un cluster real

# con 10
#> cboot$bootmean
#[1] 0.3088670 0.5064920 0.7472694 0.6651710 0.7724174 0.4300000 0.7302143 0.4281537 0.5654742 0.7102174
#> cboot$bootbrd
#[1] 96 45 13 26 23 57 25 80 30 24

#11
#> cboot$bootmean
#[1] 0.7426663 0.8370821 0.6300000 0.7738507 0.9691200 0.9535289 0.6633597 0.8359474 0.8439943 0.6074188 0.9151006
#> cboot$bootbrd
#[1] 19 17 37  6  0  3 32 18  2 15  3

#12
#> cboot$bootmean
#[1] 0.6815247 0.9459983 0.8931892 0.9000083 0.7220918 0.6471672 0.7953989 0.9257892 0.7834951 0.6100000 0.8020114
#[12] 0.7892943
#> cboot$bootbrd
#[1] 17  1  6  0 14 48  0  0  2 39  8 22

#13
#> cboot$bootmean
#[1] 0.6300000 0.6235508 0.9181978 0.8832232 0.8032844 0.8909191 0.9516792 0.9049010 0.3403012 0.7605699 0.8059354
#[12] 0.8894494 0.7436811
#> cboot$bootbrd
#[1] 37 15  0  6 11  1  1 10 86 18  6  0 37


kbest.p<-12#mas estables 12
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,count = TRUE, 
                   krange=kbest.p, seed=7777777)
groups <- cboot$result$partition
#print_clusters(cboot$result$partition, kbest.p) Por si quiero ver el contenido
summary_clusters(cboot$result$partition, kbest.p)
cboot$bootmean 
cboot$bootbrd #veces disuelto
cboot$bootrecover #veces recuperado


#Visualizacion y analicis
data = data[-4387,]#fila unica y rara BORRADA
clusplot(pmatrix[-4387,], cboot$result$partition[-4387],color=TRUE, shade=TRUE, labels=2, lines=0)

