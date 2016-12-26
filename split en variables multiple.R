partir<-function(lista,parts){

  return(split(unlist(lista),c(1:parts)))
}
partes <-4
df.pages <- data.frame()
#df.pages.partes <- data.frame(matrix(ncol = 5, nrow = 0))
#colnames(df.pages.partes) <- c("mean", "median", "std", "articlename","parte")
df.pages.partes <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df.pages.partes) <- c("mean", "std", "articlename","parte","idarticle")
for(i in 1:partes) {
  df.pages[[paste("mean",i)]]<-0
  #df.pages[[paste("median",i)]]<-0
  df.pages[[paste("std",i)]]<-0
}
df.pages[["articlename"]]<-""
index<-1
listapartes<-list()
for(i in seq_along(revisionindays)) {
  see<-partir(revisionindays[[i]][1,],partes)

  for(j in seq_along(see)) {
    media <- mean(unlist(see[j]))
    mediana <-median(unlist(see[j]))
    std <-sd(unlist(see[j]))
    df.pages[i,paste("mean",j)]<- media
    #df.pages[i,paste("median",j)]<- mediana
    df.pages[i,paste("std",j)]<- std
    df.pages[i,"articlename"]<- articlenames[i]
#    df.pages.partes[index,c("mean", "median", "std", "articlename","parte")] <- c(media,mediana,std,articlenames[i],j)
    df.pages.partes[index,c("mean", "std", "articlename","parte","idarticle")] <- c(media,std,articlenames[i],j,i)
        index<-index+1
  }
  listapartes[i]<-list(see)
}


#see <- data.frame(t(revisionindays[[1]][1,]))
#see$names <- rownames(see)
#colnames(see)<-c("Revisions","Date")
#see$Date<-as.Date(see$Date)
#ggplot(see) + geom_line(aes(Date, Revisions))  + xlab("") + ylab("Daily Views")
#unlist(revisionindays[[1]][1,],use.names = FALSE)