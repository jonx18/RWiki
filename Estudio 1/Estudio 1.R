install.packages(fpc)
install.packages(ggplot2)
install.packages(reshape2)
install.packages(pbapply)
install.packages(data.table)
library(fpc)
library(ggplot2)
library(reshape2)
library(pbapply)
library(data.table)
set.seed(7777777) #seed del estudio
numero.paginas=3
datos.estudio1<-df.articulos[sample(length(df.articulos),numero.paginas)]
df.big.estudio1<-rbindlist(datos.estudio1[unlist(pblapply(datos.estudio1,function(x){dim(x)[2]==34}))])#Rapido
df.big.estudio1<- as.data.frame(df.big.estudio1)
df.big.estudio1 <-df.big.estudio1[, colSums(df.big.estudio1 != 0) > 0]
#Seleccion de dataset a utilizar. En este estudio no se usan categorias o variables derivadas
data<-df.big.estudio1[,!colnames(df.big.estudio1)%in%c("stylesum","categories")]


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

set.seed(5793) #seed del estudio
numero.paginas=4
datos.estudio1<-df.articulos[sample(length(df.articulos),numero.paginas)]
df.big.estudio1<-rbindlist(datos.estudio1[unlist(pblapply(datos.estudio1,function(x){dim(x)[2]==34}))])#Rapido
df.big.estudio1<- as.data.frame(df.big.estudio1)
df.big.estudio1 <-df.big.estudio1[, colSums(df.big.estudio1 != 0) > 0]
#Seleccion de dataset a utilizar. En este estudio no se usan categorias o variables derivadas
data<-df.big.estudio1[,!colnames(df.big.estudio1)%in%c("stylesum","categories")]


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

set.seed(7777) #seed del estudio
numero.paginas=5
datos.estudio1<-df.articulos[sample(length(df.articulos),numero.paginas)]
df.big.estudio1<-rbindlist(datos.estudio1[unlist(pblapply(datos.estudio1,function(x){dim(x)[2]==34}))])#Rapido
df.big.estudio1<- as.data.frame(df.big.estudio1)
df.big.estudio1 <-df.big.estudio1[, colSums(df.big.estudio1 != 0) > 0]
#Seleccion de dataset a utilizar. En este estudio no se usan categorias o variables derivadas
data<-df.big.estudio1[,!colnames(df.big.estudio1)%in%c("stylesum","categories")]


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