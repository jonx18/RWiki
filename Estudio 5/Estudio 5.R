install.packages(fpc)
install.packages(ggplot2)
install.packages(reshape2)
install.packages(cluster) 
install.packages(arules)
install.packages(arulesViz)
library(fpc)
library(ggplot2)
library(reshape2)
library(cluster) 
library(arules)
library(arulesViz)

semilla<-7777777
set.seed(semilla) #seed del estudio
numero.revisiones=10000
#Seleccion de dataset a utilizar. En este estudio no se usan categorias o variables derivadas
data<-df.big.evol[sample(nrow(df.big.evol),numero.revisiones),!colnames(df.big.evol)%in%c()]
inicial <-data

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


# #Calculando cuantos K clusters usar
# #Usando el criterio de indice de Calinski-Harabasz.
# #(Promedio entre la varianza de clusters entre si y y el total de la varianza interna de los clusters.) )
# system.time(clustering.ch <- kmeansruns(pmatrix, krange=1:20, criterion="ch",algorithm="MacQueen"))
# clustering.ch$bestk #K recomendado por CH
# #Usando el criterio de "Average silhouette width"
# system.time(clustering.asw <- kmeansruns(pmatrix, krange=1:20, criterion="asw",algorithm="MacQueen"))#Criterio asw(average silhouette width)
# clustering.asw$bestk #K recomendado por ASW
# #Grafico para observar posibles K
# critframe <- data.frame(k=1:20, ch=scale(clustering.ch$crit),
#                         asw=scale(clustering.asw$crit))
# critframe <- melt(critframe, id.vars=c("k"),
#                   variable.name="measure",
#                   value.name="score")
# 
# ggplot(critframe, aes(x=k, y=score, color=measure)) +
#   geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
#   scale_x_continuous(breaks=1:20, labels=1:20)

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

clusplot(pmatrix, cboot$result$partition,color=TRUE, shade=TRUE, labels=2, lines=0)


cboot.old<-cboot
data<-data[cboot$result$partition==3,]

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

clusplot(pmatrix, cboot$result$partition,color=TRUE, shade=TRUE, labels=2, lines=0)

data<-data[cboot$result$partition!=3,]
#Escalo variables numericas
#Con esto busco depender menos de la escala de valores, SOLO NUMERICOS por utulizar clustering
pmatrix <- scale(data[,numericVars])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

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

clusplot(pmatrix, cboot$result$partition,color=TRUE, shade=TRUE, labels=2, lines=0)

act.normal<-data[cboot$result$partition==2,]
act.eliminacion<-data[cboot$result$partition==3,]
act.recuperacion<-data[cboot$result$partition==1,]

data<-inicial

want <-rbind(data,act.normal) #rbind the columns
#use !duplicated fromLast = FALSE and fromLast = TRUE to get unique rows.
want<-want[!duplicated(want,fromLast = FALSE)&!duplicated(want,fromLast = TRUE),] 

data<-want

#Escalo variables numericas
#Con esto busco depender menos de la escala de valores, SOLO NUMERICOS por utulizar clustering
pmatrix <- scale(data[,numericVars])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

kbest.p<-2#mas estables 12
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,count = TRUE, 
                   krange=kbest.p, seed=semilla,algorithm="MacQueen")
groups <- cboot$result$partition
#print_clusters(cboot$result$partition, kbest.p) Por si quiero ver el contenido
summary_clusters(cboot$result$partition, kbest.p)
cboot$bootmean 
cboot$bootbrd #veces disuelto
cboot$bootrecover #veces recuperado

clusplot(pmatrix, cboot$result$partition,color=TRUE, shade=TRUE, labels=2, lines=0)
act.eliminacion<-data[cboot$result$partition==1,]
act.recuperacion<-data[cboot$result$partition==2,]


data<-act.eliminacion

#Generacion de transacciones
estilos <- c("sup","infobox","italic","heading3","heading4","sub","blockquote","heading2","reference",
             "numberedelement","wikitable","file","nowiki","blod","internal","s","heading5","external",
             "small","bulletedelement","indent2","big","cite","italicblod","includeonly","indent1") 
#Generacion de transacciones
df.factorizado<- data[,estilos]
for(e in estilos){
  df.factorizado[,e][df.factorizado[,e]== 0]<-NA
  df.factorizado[,e] <- cut(df.factorizado[,e], breaks=c(-Inf,-10,-2,0, 2, 10, Inf), labels=c("-Alto-","-Medio-","-Bajo-","+Bajo+","+Medio+","+Alto+"))
  #str(df.factorizado[,e])
}
trans <- as(df.factorizado, "transactions")
summary(trans)

#Eclat 1

itemsets <- eclat(trans, parameter=list(support=0.1,minlen=2, tidLists = TRUE))
#Support es el soporte minimo aceptado para cada estudio.
#Minlen es el numero minimo de elementos que se quiere en los resultados elegidos, 2 para evitar resultado de un unico estilo
#TidLists nos permite de requerirlo obtener informacion de en que revisiones se originaron que resultados.

#Limpieza de repetidos y ordenado
subset.matrix <- is.subset(itemsets, itemsets)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
itemsets.pruned <- itemsets[!redundant]
itemsets<-itemsets.pruned
itemsets.sorted <- sort(itemsets)

#inspeccion de los primeros 150 elementos con mayor soporte
#inspect(itemsets.sorted[1:150])
inspect(itemsets.sorted) #descomentar para analizar el total


#informacion de los elementos utilizados de requerirse descomentar
#dim(tidLists(itemsets))
#as(tidLists(itemsets), "list")

#Apriori 1

rules <- apriori(trans, parameter=list(supp=0.1,conf=0.01, minlen=2))
#Supp y conf representan los valores minimos tenidos en cuenta en los resultados para soporte y confidencia respectivamente

#Eliminacion de repetidos y ordenamiento
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
length(rules)
#Ordenamiento por Confidencia
rules.sorted <- sort(rules, by="confidence", decreasing=TRUE)
inspect(rules.sorted)
#ordenamiento en primer grado por confidencia y en segundo por elevacion.
#df.rules <- as(rules, "data.frame") 
#df.rules[order(df.rules$confidence,df.rules$lift, decreasing = TRUE), ]


data<-act.recuperacion

#Generacion de transacciones
estilos <- c("sup","infobox","italic","heading3","heading4","sub","blockquote","heading2","reference",
             "numberedelement","wikitable","file","nowiki","blod","internal","s","heading5","external",
             "small","bulletedelement","indent2","big","cite","italicblod","includeonly","indent1") 
#Generacion de transacciones
df.factorizado<- data[,estilos]
for(e in estilos){
  df.factorizado[,e][df.factorizado[,e]== 0]<-NA
  df.factorizado[,e] <- cut(df.factorizado[,e], breaks=c(-Inf,-10,-2,0, 2, 10, Inf), labels=c("-Alto-","-Medio-","-Bajo-","+Bajo+","+Medio+","+Alto+"))
  #str(df.factorizado[,e])
}
trans <- as(df.factorizado, "transactions")
summary(trans)

#Eclat 2

itemsets <- eclat(trans, parameter=list(support=0.1,minlen=2, tidLists = TRUE))
#Support es el soporte minimo aceptado para cada estudio.
#Minlen es el numero minimo de elementos que se quiere en los resultados elegidos, 2 para evitar resultado de un unico estilo
#TidLists nos permite de requerirlo obtener informacion de en que revisiones se originaron que resultados.

#Limpieza de repetidos y ordenado
subset.matrix <- is.subset(itemsets, itemsets)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
itemsets.pruned <- itemsets[!redundant]
itemsets<-itemsets.pruned
itemsets.sorted <- sort(itemsets)

#inspeccion de los primeros 150 elementos con mayor soporte
#inspect(itemsets.sorted[1:150])
inspect(itemsets.sorted) #descomentar para analizar el total


#informacion de los elementos utilizados de requerirse descomentar
#dim(tidLists(itemsets))
#as(tidLists(itemsets), "list")

#Apriori 2

rules <- apriori(trans, parameter=list(supp=0.1,conf=0.01, minlen=2))
#Supp y conf representan los valores minimos tenidos en cuenta en los resultados para soporte y confidencia respectivamente

#Eliminacion de repetidos y ordenamiento
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
length(rules)
#Ordenamiento por Confidencia
rules.sorted <- sort(rules, by="confidence", decreasing=TRUE)
inspect(rules.sorted)
#ordenamiento en primer grado por confidencia y en segundo por elevacion.
#df.rules <- as(rules, "data.frame") 
#df.rules[order(df.rules$confidence,df.rules$lift, decreasing = TRUE), ]


data<-act.normal

#Generacion de transacciones
estilos <- c("sup","infobox","italic","heading3","heading4","sub","blockquote","heading2","reference",
             "numberedelement","wikitable","file","nowiki","blod","internal","s","heading5","external",
             "small","bulletedelement","indent2","big","cite","italicblod","includeonly","indent1") 
#Generacion de transacciones
df.factorizado<- data[,estilos]
for(e in estilos){
  df.factorizado[,e][df.factorizado[,e]== 0]<-NA
  df.factorizado[,e] <- cut(df.factorizado[,e], breaks=c(-Inf,-10,-2,0, 2, 10, Inf), labels=c("-Alto-","-Medio-","-Bajo-","+Bajo+","+Medio+","+Alto+"))
  #str(df.factorizado[,e])
}
trans <- as(df.factorizado, "transactions")
summary(trans)

#Eclat 3

itemsets <- eclat(trans, parameter=list(support=0.001,minlen=2, tidLists = TRUE))
#Support es el soporte minimo aceptado para cada estudio.
#Minlen es el numero minimo de elementos que se quiere en los resultados elegidos, 2 para evitar resultado de un unico estilo
#TidLists nos permite de requerirlo obtener informacion de en que revisiones se originaron que resultados.

#Limpieza de repetidos y ordenado
subset.matrix <- is.subset(itemsets, itemsets)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
itemsets.pruned <- itemsets[!redundant]
itemsets<-itemsets.pruned
itemsets.sorted <- sort(itemsets)

#inspeccion de los primeros 150 elementos con mayor soporte
#inspect(itemsets.sorted[1:150])
inspect(itemsets.sorted) #descomentar para analizar el total


#informacion de los elementos utilizados de requerirse descomentar
#dim(tidLists(itemsets))
#as(tidLists(itemsets), "list")

#Apriori 3

rules <- apriori(trans, parameter=list(supp=0.001,conf=0.01, minlen=2))
#Supp y conf representan los valores minimos tenidos en cuenta en los resultados para soporte y confidencia respectivamente

#Eliminacion de repetidos y ordenamiento
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
length(rules)
#Ordenamiento por Confidencia
rules.sorted <- sort(rules, by="confidence", decreasing=TRUE)
inspect(rules.sorted)
#ordenamiento en primer grado por confidencia y en segundo por elevacion.
#df.rules <- as(rules, "data.frame") 
#df.rules[order(df.rules$confidence,df.rules$lift, decreasing = TRUE), ]
