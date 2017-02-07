install.packages(arules)
install.packages(arulesViz)
library(arules)
library(arulesViz)

#Generacion de transacciones
estilos <- c("sup","infobox","italic","heading3","heading4","sub","blockquote","heading2","reference",
             "numberedelement","wikitable","file","nowiki","blod","internal","s","heading5","external",
             "small","bulletedelement","indent2","big","cite","italicblod","includeonly","indent1") 
df.factorizado<- df.big.evol[,estilos]
for(e in estilos){
  df.factorizado[,e][df.factorizado[,e]== 0]<-NA
  df.factorizado[,e] <- cut(df.factorizado[,e], breaks=c(-Inf,-10,-2,0, 2, 10, Inf), labels=c("-Alto-","-Medio-","-Bajo-","+Bajo+","+Medio+","+Alto+"))
  #str(df.factorizado[,e])
}
trans <- as(df.factorizado, "transactions")
summary(trans)

#Eclat 1

itemsets <- eclat(trans, parameter=list(support=0.01,minlen=2, tidLists = TRUE))
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

#Eclat 2

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

#Apriori 1

rules <- apriori(trans, parameter=list(supp=0.01,conf=0.01, minlen=2))
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
df.rules <- as(rules, "data.frame") 
df.rules[order(df.rules$confidence,df.rules$lift, decreasing = TRUE), ]

#Apriori 2

rules <- apriori(trans, parameter=list(supp=0.001,conf=0.001, minlen=2,maxlen=5))
#Supp y conf representan los valores minimos tenidos en cuenta en los resultados para soporte y confidencia respectivamente
#Minlen y MaxLen limitan el rango de los elementos que puede contener cada resultado

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

