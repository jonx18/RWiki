#generacion dinamica de dataframes
#Obtengo json files en un directorio
library("jsonlite")#Cargo libreria de JSON
files <- list.files(path = "C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\exports 285\\" ,pattern="*.txt")
files <- lapply(files, function(x) paste("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\exports 285\\",x,sep=""))
jsonfile <- lapply(files, readLines)
articlenames <- lapply(jsonfile,function(x) x[8])

#Obtengo datos de estilos 
json <- lapply(jsonfile,function(x) {fromJSON(x[2])})

#Creo data frame con datos de estilos
df.articulos <- lapply(json,function(x) {data.frame(x)})
#Obtengo datos de fecha hora y peso
json <- lapply(jsonfile,function(x) {fromJSON(x[6])})
#normalizo tamaños
normalizar.df.list<-function(dataf,lista){
  if(dim(dataf)[1] != length(lista)){
    if(dim(dataf)[1] > length(lista)){
      dataf <- dataf[1:length(lista),]
    }else{
      length(lista)<-dim(dataf)[1]
    }
  } 
  return(list(dataf,lista))
}
for(i in seq_along(df.articulos)) {
#          if(dim(df.articulos[[i]])[1] != length(json[[i]])){
#            if(dim(df.articulos[[i]])[1] > length(json[[i]])){
#              df.articulos[[i]] <- df.articulos[[i]][1:length(json[[i]]),]
#            }else{
#              length(json[[i]])<-dim(df.articulos[[i]])[1]
#            }
#          } 
     resultado<- normalizar.df.list(df.articulos[[i]],json[[i]])# en caso de problemas de tamaño
     df.articulos[[i]]<-resultado[[1]]
     json[[i]]<-resultado[[2]]
        df.articulos[[i]]$content_size<-unlist(json[[i]])#Cargo en el data frame pesos
        df.articulos[[i]]$date <- as.Date(names(json[[i]]))#Cargo en el data frame fechas
       }

#Obtengo datos de categorias
json <- lapply(jsonfile,function(x) {fromJSON(x[7])})
for(i in seq_along(df.articulos)) {
  df.articulos[[i]]$categories<-unlist(lapply(json[[i]],function(x) length(unlist(x))))#Cargo en el data frame pesos
}

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
json <- lapply(jsonfile,function(x) {fromJSON(x[5])})
revisionindays<-list()
for(i in seq_along(df.articulos)) {
  lista<-unlist(distribution_revision_list(json[[i]]))
  resultado<- normalizar.df.list(df.articulos[[i]],lista)
  df.articulos[[i]]<-resultado[[1]]
  lista<-resultado[[2]]
  df.articulos[[i]]$revisions<-lista

  #Obtengo revisiones dia a dia
  names(json[[i]])<-as.Date(names(json[[i]]))
  #days <- seq(min(as.Date(names(json[[i]]))), max(as.Date(names(json[[i]]))), by="days") last revision
  days <- seq(min(as.Date(names(json[[i]]))), Sys.Date(), by="days")
  test<- as.data.frame(matrix(0, ncol = length(days), nrow = 1)) 
  colnames(test)<-days
  test[,names(json[[i]])]<-json[[i]]
  revisionindays[[i]] <-test
  rm(test)
  rm(days)
  rm(lista)
}

#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(df.articulos[[1]]),c('rgroup'))
#Categoricas
catVars <- vars[sapply(df.articulos[[1]][,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(df.articulos[[1]][,vars],class) %in%  c('numeric','integer')]

#suma de estilos
for(i in seq_along(df.articulos)) {
  df.articulos[[i]]$stylesum <- unlist(rowSums(df.articulos[[i]][,colnames(df.articulos[[i]])%in%numericVars[!numericVars%in%c("content_size","categories","revisions")]]))
  
}
#agrego nombres
json <- lapply(jsonfile,function(x) x[8])
for(i in seq_along(df.articulos)) {
  df.articulos[[i]]$pagename<-json[[i]]
}

#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(df.articulos[[1]]),c('rgroup'))
#Categoricas
catVars <- vars[sapply(df.articulos[[1]][,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(df.articulos[[1]][,vars],class) %in%  c('numeric','integer')]

dftoevolutiondf <- function(data) {
 # print(dim(data))
  if(dim(data)[1]>1){
  #  print("largo")
    resta<-data[-dim(data)[1],]
    resta<-rbind(0,resta)
    data$link <- c(1:dim(data)[1])
    resta$link <- c(1:dim(resta)[1])
    #resta <- data-resta no existe
    M <- merge(data,resta,by="link")
    
    S <- M[,grepl("*\\.x$",names(M))] - M[,grepl("*\\.y$",names(M))]
   # print(dim(M))
    #print(M)
    #print(dim(S))
    result <- cbind(M[,1,drop=FALSE],S)
    colnames(result)<-substr(colnames(result),1,nchar(colnames(result))-2)
    data[,"link"]=NULL
    result[,"li"]=NULL
  }
  else{
   # print("corto")
    result<-data
  }

  return(result)
}

df.articulos.evol <- lapply(df.articulos,function(x) {dftoevolutiondf(x[,numericVars])})#Creo data frame con evolucion de articulos
for(i in seq_along(df.articulos)) {
  datatmp<-df.articulos[[i]]
  datatmp[,numericVars]<-df.articulos.evol[[i]]
  df.articulos.evol[[i]]<-datatmp
}
remove(datatmp)
#Creando gran dataframe
df.big<-data.frame()
df.big.evol<-data.frame()
for(i in seq_along(df.articulos)) {
  df.big <- rbind(df.big,df.articulos[[i]])
  df.big.evol <- rbind(df.big.evol,df.articulos.evol[[i]])
}

#limpiando columnas que den 0
df.big <-df.big[, colSums(df.big != 0) > 0]
df.big.evol <-df.big.evol[, colSums(df.big.evol != 0) > 0]
for(i in seq_along(df.articulos)) {
  df.articulos[[i]] <- df.articulos[[i]][, colSums(df.articulos[[i]] != 0) > 0]
  df.articulos.evol[[i]] <- df.articulos.evol[[i]][, colSums(df.articulos.evol[[i]] != 0) > 0]
}