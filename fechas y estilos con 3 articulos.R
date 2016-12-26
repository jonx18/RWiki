library("jsonlite")#Cargo libreria de JSON
jsonfile <- list(julio="",papa="",johnny="")
jsonfile$julio <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page334671.txt")#Leo Archivo de Julio Cortazar
jsonfile$papa <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page23056.txt")#Leo Archivo del Papa
jsonfile$johnny <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page71870.txt")#Leo Archivo de Johnny Depp
#Por cada archivo tengo
#[1] Listado de fechas de revisiones
#[2] Cambios de estilo en toda la historia del articulo
#[3] Toral de revisiones
#[4] Distribucion de aporte por autor
#[5] Cantidad de revisiones dia a dia
#[6] Contenido revision a revision
#[7] Listado de nombres de categoria revision a revision
#[8] nombre del articulo
json <- list(julio=fromJSON(jsonfile$julio[2]),papa=fromJSON(jsonfile$papa[2]),
             johnny=fromJSON(jsonfile$johnny[2]))#Obtengo datos de estilos 
df.articulos <- list(julio=data.frame(json$julio),papa=data.frame(json$papa),
                     johnny=data.frame(json$johnny))#Creo data frame con datos de estilos
json <- list(julio=fromJSON(jsonfile$julio[6]),papa=fromJSON(jsonfile$papa[6]),
             johnny=fromJSON(jsonfile$johnny[6]))#Obtengo datos de fecha hora y peso
df.articulos$johnny<-df.articulos$johnny[1:(nrow(df.articulos$johnny)-1),]# en caso de problemas de tamaño
df.articulos$julio$content_size <- unlist(json$julio)#Cargo en el data frame pesos 
df.articulos$papa$content_size <- unlist(json$papa)#Cargo en el data frame pesos 
df.articulos$johnny$content_size <- unlist(json$johnny)#Cargo en el data frame pesos 
df.articulos$julio$date <- as.Date(names(json$julio))#Cargo en el data frame fechas
df.articulos$papa$date <- as.Date(names(json$papa))#Cargo en el data frame fechas
df.articulos$johnny$date <- as.Date(names(json$johnny))#Cargo en el data frame fechas
json <- list(julio=fromJSON(jsonfile$julio[7]),papa=fromJSON(jsonfile$papa[7]),
             johnny=fromJSON(jsonfile$johnny[7]))#Obtengo datos de categorias
df.articulos$julio$categories <- unlist(lapply(json$julio,function(x) length(unlist(x))))#cargo solo cantidad de categorias
df.articulos$papa$categories <- unlist(lapply(json$papa,function(x) length(unlist(x))))#cargo solo cantidad de categorias
df.articulos$johnny$categories <- unlist(lapply(json$johnny,function(x) length(unlist(x))))#cargo solo cantidad de categorias
#agrego numero de revisiones de la misma fecha
distribution_revision_list <- function(revisions) {
  result <- data.frame()
  indice<-1
  for(i  in 1:length(revisions)){
    for (j in 1:revisions[[i]]){
      result[indice,1]<-revisions[[i]]
      indice<-indice+1
    }
  }
  return(result)
}
df.articulos$julio$revisions <-unlist(distribution_revision_list(fromJSON(jsonfile$julio[5])))
df.articulos$papa$revisions <-unlist(distribution_revision_list(fromJSON(jsonfile$papa[5])))
df.articulos$johnny$revisions <-unlist(distribution_revision_list(fromJSON(jsonfile$johnny[5]))[-9657,])
#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(df.articulos$julio),c('rgroup'))
#Categoricas
catVars <- vars[sapply(df.articulos$julio[,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(df.articulos$julio[,vars],class) %in%  c('numeric','integer')]
#suma de estilos
df.articulos$julio$stylesum <- unlist(rowSums(df.articulos$julio[,colnames(df.articulos$julio)%in%numericVars[!numericVars%in%c("content_size","categories","revisions")]]))
df.articulos$papa$stylesum <- unlist(rowSums(df.articulos$papa[,colnames(df.articulos$papa)%in%numericVars[!numericVars%in%c("content_size","categories","revisions")]]))
df.articulos$johnny$stylesum <- unlist(rowSums(df.articulos$johnny[,colnames(df.articulos$johnny)%in%numericVars[!numericVars%in%c("content_size","categories","revisions")]]))

#agrego nombres
df.articulos$julio$pagename <- "julio"
df.articulos$papa$pagename <- "papa"
df.articulos$johnny$pagename <- "johnny"

#Creando dataframe de evolucion
dftoevolutiondf <- function(data) {
  resta<-data[-dim(data)[1],]
  resta<-rbind(0,resta)
  data$link <- c(1:dim(data)[1])
  resta$link <- c(1:dim(resta)[1])
  #resta <- data-resta no existe
  M <- merge(data,resta,by="link")
  
  S <- M[,grepl("*\\.x$",names(M))] - M[,grepl("*\\.y$",names(M))]
  
  result <- cbind(M[,1,drop=FALSE],S)
  colnames(result)<-substr(colnames(result),1,nchar(colnames(result))-2)
  data[,"link"]=NULL
  result[,"li"]=NULL
  return(result)
}
#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(df.articulos$julio),c('rgroup'))
#Categoricas
catVars <- vars[sapply(df.articulos$julio[,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(df.articulos$julio[,vars],class) %in%  c('numeric','integer')]

df.articulos.evol <- list(julio=dftoevolutiondf(df.articulos$julio[,numericVars]),
                          papa=dftoevolutiondf(df.articulos$papa[,numericVars]),
                          johnny=dftoevolutiondf(df.articulos$johnny[,numericVars]))#Creo data frame con evolucion de articulos
datatmp<-df.articulos$julio
datatmp[,numericVars]<-df.articulos.evol$julio
df.articulos.evol$julio<-datatmp
datatmp<-df.articulos$papa
datatmp[,numericVars]<-df.articulos.evol$papa
df.articulos.evol$papa<-datatmp
datatmp<-df.articulos$johnny
datatmp[,numericVars]<-df.articulos.evol$johnny
df.articulos.evol$johnny<-datatmp
remove(datatmp)
#Creando gran dataframe
df.big <- df.articulos$julio
df.big <- rbind(df.big,df.articulos$papa)
df.big <- rbind(df.big,df.articulos$johnny)
df.big.evol <- df.articulos.evol$julio
df.big.evol <- rbind(df.big.evol,df.articulos.evol$papa)
df.big.evol <- rbind(df.big.evol,df.articulos.evol$johnny)


#limpiando columnas que den 0
df.big <-df.big[, colSums(df.big != 0) > 0]
df.articulos$julio <- df.articulos$julio[, colSums(df.articulos$julio != 0) > 0]
df.articulos$papa <- df.articulos$papa[, colSums(df.articulos$papa != 0) > 0]
df.articulos$johnny <- df.articulos$johnny[, colSums(df.articulos$johnny != 0) > 0]
df.big.evol <-df.big.evol[, colSums(df.big.evol != 0) > 0]
df.articulos.evol$julio <- df.articulos.evol$julio[, colSums(df.articulos.evol$julio != 0) > 0]
df.articulos.evol$papa <- df.articulos.evol$papa[, colSums(df.articulos.evol$papa != 0) > 0]
df.articulos.evol$johnny <- df.articulos.evol$johnny[, colSums(df.articulos.evol$johnny != 0) > 0]