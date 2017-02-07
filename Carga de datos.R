#generacion dinamica de dataframes
#Obtengo json files en un directorio
install.packages(pbapply)
install.packages(data.table)
install.packages(jsonlite)
library(pbapply)
library(data.table)
library(jsonlite)#Cargo libreria de JSON
directorio <-"C:\\exports\\"# Indique donde estan los archivos del export
files <- list.files(path = directorio ,pattern="*.txt")
files <- pblapply(files, function(x) paste(directorio,x,sep=""))
jsonfile <- pblapply(files, readLines)
articlenames <- pblapply(jsonfile,function(x) x[8])

#Obtengo datos de estilos 
json <- pblapply(jsonfile,function(x) {fromJSON(x[2])})

#Creo data frame con datos de estilos
df.articulos <- pblapply(json,function(x) {data.frame(x)})
#Obtengo datos de fecha hora y peso
json <- pblapply(jsonfile,function(x) {fromJSON(x[6])})
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
borrables<-0
borrar<-list()
for(i in seq_along(df.articulos)) {
  #          if(dim(df.articulos[[i]])[1] != length(json[[i]])){
  #            if(dim(df.articulos[[i]])[1] > length(json[[i]])){
  #              df.articulos[[i]] <- df.articulos[[i]][1:length(json[[i]]),]
  #            }else{
  #              length(json[[i]])<-dim(df.articulos[[i]])[1]
  #            }
  #          } 
  if(dim(df.articulos[[i]])[1]==0 ||dim(df.articulos[[i]])[2]==0 || length(json[[i]])==0){
    borrables<-borrables+1
    borrar[[borrables]]<-i
  }
  else{
    
    resultado<- normalizar.df.list(df.articulos[[i]],json[[i]])# en caso de problemas de tamaño
    df.articulos[[i]]<-resultado[[1]]
    json[[i]]<-resultado[[2]]
    
    df.articulos[[i]]$content_size<-unlist(json[[i]])#Cargo en el data frame pesos
    df.articulos[[i]]$date <- as.Date(names(json[[i]]))#Cargo en el data frame fechas
  }
}
gc()
for(i in borrar) {
  df.articulos[[i]]<-NULL
  jsonfile[i]<-NULL
  articlenames[i]<-NULL
  #if(file.exists(files[i])){
  #file.remove(files[i])
  #}
}
#Obtengo datos de categorias
json <- pblapply(jsonfile,function(x) {fromJSON(x[7])})
for(i in seq_along(df.articulos)) {
  df.articulos[[i]]$categories<-unlist(pblapply(json[[i]],function(x) length(unlist(x))))#Cargo en el data frame pesos
}
gc()
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
json <- pblapply(jsonfile,function(x) {fromJSON(x[5])})
revisionindays<-list()
#borrables<-0
i.ant=1
borrar<-list()

for(i in seq_along(df.articulos)) {
  if(i<i.ant){next}
  if(dim(df.articulos[[i]])[1]==0 ||dim(df.articulos[[i]])[2]==0 || length(json[[i]])==0){
    borrables<-borrables+1
    borrar[[borrables]]<-i
  }
  else{
    lista<-unlist(distribution_revision_list(json[[i]]))
    print(i)
    # print(dim(df.articulos[[i]]))
    # print(length(json[[i]]))
    resultado<- normalizar.df.list(df.articulos[[i]],lista)
    df.articulos[[i]]<-resultado[[1]]
    lista<-resultado[[2]]
    #  print(dim(df.articulos[[i]]))
    #  print(length(json[[i]]))
    
    df.articulos[[i]]$revisions<-lista
    
    #Obtengo revisiones dia a dia
    names(json[[i]])<-as.Date(names(json[[i]]))
    #days <- seq(min(as.Date(names(json[[i]]))), max(as.Date(names(json[[i]]))), by="days") last revision
    days <- seq(min(as.Date(names(json[[i]]))), Sys.Date(), by="days")
    test<- as.data.frame(matrix(0, ncol = length(days), nrow = 1)) 
    colnames(test)<-days
    test[,names(json[[i]])]<-json[[i]]
    revisionindays[[i]] <-test
    test<-NULL
    days<-NULL
    lista<-NULL
    if(i%%50000==0){
      break
      rm(test)
      rm(days)
      rm(lista)
      gc()
      }
  }
}
i.ant=i
rm(test)
rm(days)
rm(lista)
gc()
for(i in borrar) {
  if(!is.null(i)){
    df.articulos[[i]]<-NULL
    jsonfile[i]<-NULL
    articlenames[i]<-NULL
    #if(file.exists(files[i])){
    #file.remove(files[i])
    #}
  }
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
gc()
#agrego nombres
#borrables<-0
borrar<-list()
json <- pblapply(jsonfile,function(x) x[8])
gc()
for(i in seq_along(df.articulos)) {
  if(dim(df.articulos[[i]])[1]==0 ||dim(df.articulos[[i]])[2]==0 || length(json[[i]])==0){
    borrables<-borrables+1
    borrar[[borrables]]<-i
  }
  else{
    # print(dim(df.articulos[[i]]))
    df.articulos[[i]]$pagename<-json[[i]]
    
  }
}
for(i in borrar) {
  if(!is.null(i)){
    df.articulos[[i]]<-NULL
    jsonfile[i]<-NULL
    articlenames[i]<-NULL
    #if(file.exists(files[i])){
    #file.remove(files[i])
    #}
  }
}
rm(json)
rm(jsonfile)
rm(borrar)
rm(i)
rm(i.ant)
rm(resultado)
rm(revisionindays)
gc()
#Obtengo columnas por tipo
#Todas
vars <- setdiff(colnames(df.articulos[[1]]),c('rgroup'))
#Categoricas
catVars <- vars[sapply(df.articulos[[1]][,vars],class) %in% c('factor','character')]
#Numericas
numericVars <- vars[sapply(df.articulos[[1]][,vars],class) %in%  c('numeric','integer')]

dftoevolutiondf <- function(data) {
  data<-as.data.frame(data)
  #print(data)
  #print(is.null(data))
  #print(dim(data))
  if(! is.null(data)){
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
}
#df.articulos[unlist(lapply(df.articulos,function(x) {!(colnames(x) %in% numericVars)}))]<-NULL #elimino vacios
df.articulos.evol <- pblapply(df.articulos,function(x) {
  if(all(numericVars%in%colnames(x))){
    dftoevolutiondf(x[,numericVars])
    
  }
}

)#Creo data frame con evolucion de articulos
gc()
for(i in seq_along(df.articulos)) {
  datatmp<-df.articulos[[i]]
  datatmp[,numericVars]<-df.articulos.evol[[i]]
  df.articulos.evol[[i]]<-datatmp
  
}
remove(datatmp)
gc()
#Creando gran dataframe
df.big<-data.frame()
df.big.evol<-data.frame()
#for(i in seq_along(df.articulos)) {
#  print(i)
#  if(i==1||length(colnames(df.big))==length(colnames(df.articulos.evol[[i]]))&&length(colnames(df.big.evol))==length(colnames(df.articulos.evol[[i]]))){
#    df.big <- rbind(df.big,df.articulos[[i]])
#   df.big.evol <- rbind(df.big.evol,df.articulos.evol[[i]])
#  }
#}LENTO
df.big<-rbindlist(df.articulos[unlist(pblapply(df.articulos,function(x){dim(x)[2]==34}))])#Rapido
df.big<- as.data.frame(df.big)
df.big.evol<-rbindlist(df.articulos.evol[unlist(pblapply(df.articulos.evol,function(x){dim(x)[2]==34}))])
df.big.evol<- as.data.frame(df.big.evol)
gc()
#limpiando columnas que den 0
df.big <-df.big[, colSums(df.big != 0) > 0]
df.big.evol <-df.big.evol[, colSums(df.big.evol != 0) > 0]
#for(i in seq_along(df.articulos)) {
#  print(i)
#  df.articulos[[i]] <- df.articulos[[i]][, colSums(df.articulos[[i]] != 0) > 0]
#  df.articulos.evol[[i]] <- df.articulos.evol[[i]][, colSums(df.articulos.evol[[i]] != 0) > 0]
#}LENTO
df.articulos<-pblapply(df.articulos,function(x){x[, colSums(x != 0) > 0]})#Rapido
df.articulos.evol<-pblapply(df.articulos.evol,function(x){x[, colSums(x != 0) > 0]})
