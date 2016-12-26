library(fpc)
library("ggplot2")#Cargo libreria para graficar
data<-df.big
#data<-df.articulos$johnny
set.seed(729375)
vars <- setdiff(colnames(data),
                c('rgroup'))
catVars <- vars[sapply(data[,vars],class) %in%
                  c('factor','character')]#obtengo variables categoricas
numericVars <- vars[sapply(data[,vars],class) %in%
                      c('numeric','integer')]#obtengo variables numericas
#vars.to.use <- colnames(data)[-1]
#Con esto busco depender menos de la escala de valores, SOLO NUMERICOS (pierdo pagename)
pmatrix <- scale(data[,numericVars])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")
kbest.p<-5#numero de clusters

#impresor de clusters
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

#corre kmeans con 5 clusters,100 comienzos aleatorios y 100 interaciones maximo
pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=100)
summary(pclusters)
pclusters$centers
pclusters$size
groups <- pclusters$cluster
print_clusters(groups, kbest.p)
summary_clusters(groups, kbest.p)

#data[4387,]
#[1] "cluster X"  acto de vandalismo muy raro
#big cite infobox italic heading2 blod sup file indent1 sub heading5 blockquote bulletedelement wikitable heading3
#4387   0    0       0     27      330   55 110    0    1870 275        0        770               0       550        0
#nowiki internal italicblod external reference numberedelement indent2   s heading4 small content_size categories
#4387    495      990         27      165         0             165       0 165        0   330      4116640          0


#Calculando K

clustering.ch <- kmeansruns(pmatrix, krange=1:20, criterion="ch")#criterio CH
clustering.ch$bestk
clustering.asw <- kmeansruns(pmatrix, krange=1:20, criterion="asw")#Criterio asw(average silhouette width)
clustering.asw$bestk
clustering.ch$crit
clustcrit$crit
#ploting
critframe <- data.frame(k=1:20, ch=scale(clustering.ch$crit),
                        asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")

ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:20, labels=1:20)
summary(clustering.ch)


#clusterboot
kbest.p<-12#mas estables 12
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,
                   krange=kbest.p, seed=15555)
groups <- cboot$result$partition
print_clusters(cboot$result$partition, kbest.p)
summary_clusters(cboot$result$partition, kbest.p)
cboot$bootmean
cboot$bootbrd


#asignando puntos nuevos a un cluster
assign_cluster <- function(newpt, centers, xcenter=0, xscale=1) {
  xpt <- (newpt - xcenter)/xscale
  dists <- apply(centers, 1, FUN=function(c0){sqr_edist(c0, xpt)})
  which.min(dists)
}
