library(fpc)
library(reshape2)
library(ggplot2)
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

#Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")
hcd <- as.dendrogram(pfit)
#plot(pfit)
plot(hcd, main="Main")
plot(cut(hcd, h=75)$upper, 
     main="Upper tree of cut at h=75")
plot(cut(hcd, h=75)$lower[[2]], 
     main="Second branch of lower tree with cut at h=75")
rect.hclust(pfit, k=5)#asumiendo que sean 5 clusters
groups <- cutree(pfit, k=5)
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(data[labels==i,numericVars])
  }
}
print_clusters(groups, 5)



princ <- prcomp(pmatrix)
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp]
project.plus <- cbind(as.data.frame(project),
                      cluster=as.factor(groups),
                      pagename=data$pagename)
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=pagename),
            hjust=0, vjust=1)

#BOOTSTRAP EVALUATION OF CLUSTERS

#As a rule of thumb, clusters with a stability value less than 0.6 should be considered unstable.
#Values between 0.6 and 0.75 indicate that the cluster is measuring a pattern in the data, 
#but there isn't high certainty about which points should be clustered together. 
#Clusters with stability values above about 0.85 can be considered highly stable (they're likely to be real clusters).

kbest.p<-5#numero de clusters
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI,
                            method="ward.D", k=kbest.p)
summary(cboot.hclust$result)
groups<-cboot.hclust$result$partition
print_clusters(groups, kbest.p)
cboot.hclust$bootmean#estabilidad por cluster
cboot.hclust$bootbrd#cantidad de clusters disueltos


#PICKING THE NUMBER OF CLUSTERS(Calinski-Harabasz(CH))
#Total within sum of squares
sqr_edist <- function(x, y) {#distancia de cuadrados entre dos vectores
  sum((x-y)^2)
}

wss.cluster <- function(clustermat) {
  c0 <- apply(clustermat, 2, FUN=mean)
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))
}

wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))
  wsstot
}

totss <- function(dmatrix) {#suma total de los cuadrados
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}

ch_criterion <- function(dmatrix, kmax, method="kmeans") {#calculo de CH
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1] # number of rows.
  totss <- totss(dmatrix)
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))
  for(k in 2:kmax) {
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else { # hclust
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward.D")
      print(paste("vuelta", k))
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss
  crit.num <- bss/(0:(kmax-1))
  crit.denom <- wss/(npts - 1:kmax)
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)
}
#En uso-------------------------------------------
clustcrit <- ch_criterion(pmatrix, 10, method="hclust")
clustcrit$crit[which(clustcrit$crit==Inf)] = NA #limpio infinitos

critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),
                        wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)