library("arules")
dim(trans)
summary(trans)
#----------------------------------------------------------------------------------------------------
itemsets <- eclat(trans)
inspect(itemsets)
itemsets.sorted <- sort(itemsets)
inspect(itemsets.sorted)
#----------------------------------------------------------------------------------------------------
itemsets <- eclat(trans, parameter=list(support=0.01,minlen=2))
inspect(itemsets)
itemsets.sorted <- sort(itemsets)
inspect(itemsets.sorted)
#----------------------------------------------------------------------------------------------------
itemsets <- eclat(trans, parameter=list(support=0.01,minlen=2, tidLists = TRUE))#me retorna la lista de los elementos usados jejeje
inspect(itemsets)
subset.matrix <- is.subset(itemsets, itemsets)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
itemsets.pruned <- itemsets[!redundant]
itemsets<-itemsets.pruned
inspect(itemsets)
itemsets.sorted <- sort(itemsets)
inspect(itemsets.sorted[1:150])
dim(tidLists(itemsets))
## Coerce tidLists to list.
as(tidLists(itemsets), "list")
## Inspect visually.
#image(tidLists(itemsets))#no muy util con tantas transacciones
#----------------------------------------------------------------------------------------------------
itemsets <- eclat(trans, parameter=list(support=0.01,minlen=2, tidLists = TRUE, target="frequent itemsets"))
inspect(itemsets)
itemsets.sorted <- sort(itemsets)
inspect(itemsets.sorted)
dim(tidLists(itemsets))
## Coerce tidLists to list.
as(tidLists(itemsets), "list")
#----------------------------------------------------------------------------------------------------
itemsets <- eclat(trans, parameter=list(support=0.01,minlen=2, tidLists = TRUE, target="maximally frequent itemsets"))
inspect(itemsets)
itemsets.sorted <- sort(itemsets)
inspect(itemsets.sorted)
dim(tidLists(itemsets))
## Coerce tidLists to list.
as(tidLists(itemsets), "list")
#----------------------------------------------------------------------------------------------------
itemsets <- eclat(trans, parameter=list(support=0.01,minlen=2, tidLists = TRUE, target="closed frequent itemsets"))
itemsets.sorted <- sort(itemsets)
inspect(itemsets.sorted)
dim(tidLists(itemsets))
## Coerce tidLists to list.
as(tidLists(itemsets), "list")