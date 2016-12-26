library("arules")
datos <- df.big.evol

vars <- setdiff(colnames(datos),c('rgroup'))
#Categoricas
catVars <- vars[sapply(datos[,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(datos[,vars],class) %in%  c('numeric','integer')]

#Escalo variables numericas
#Con esto busco depender menos de la escala de valores, SOLO NUMERICOS (pierdo pagename)
pmatrix <- scale(datos[,numericVars])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")
#datos <- sapply(datos,as.factor)
datos <- sapply(pmatrix,discretize)
trans <- as(pmatrix, "transactions")

dim(datos)
summary(datos)
itemsets <- eclat(trans)
