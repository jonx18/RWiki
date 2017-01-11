#experimento 5 generacion de transacciones
library("arules")
estilos <- c("sup","infobox","italic","heading3","heading4","sub","blockquote","heading2","reference",
             "numberedelement","wikitable","file","nowiki","blod","internal","s","heading5","external",
             "small","bulletedelement","indent2","big","cite","italicblod","includeonly","indent1")   
df.factorizado<- df.big.evol[,estilos]#experimento 5
for(e in estilos){
  df.factorizado[,e][df.factorizado[,e]==0]<-NA
  df.factorizado[,e][df.factorizado[,e]<0]<-"-Eliminado"
  df.factorizado[,e][df.factorizado[,e]>0]<-"+Agregado"
  df.factorizado[,e]<-as.factor(df.factorizado[,e])
}
trans <- as(df.factorizado, "transactions")
summary(trans)
df.factorizado<- df.big.evol[,estilos]#experimento 6
for(e in estilos){
  df.factorizado[,e][df.factorizado[,e]== 0]<-NA
#  df.factorizado[,e][(df.factorizado[,e]> 0) & (df.factorizado[,e]<= 2)]<-"+Bajo+"
#  df.factorizado[,e][(df.factorizado[,e]> 2) & (df.factorizado[,e]<= 10)]<-"+Medio+"
#  df.factorizado[,e][(df.factorizado[,e]> 10)]<-"+Alto+"
#  df.factorizado[,e][(df.factorizado[,e]< 0) & (df.factorizado[,e]>= -2)]<-"-Bajo-"
#  df.factorizado[,e][(df.factorizado[,e]< -2) & (df.factorizado[,e]>= -10)]<-"-Medio-"
#  df.factorizado[,e][(df.factorizado[,e]< -10)]<-"-Alto-"
 # df.factorizado[,e][df.factorizado[,e]< 0]<-"-1"
#  df.factorizado[,e]<-as.factor(df.factorizado[,e])
  df.factorizado[,e] <- cut(df.factorizado[,e], breaks=c(-Inf,-10,-2,0, 2, 10, Inf), labels=c("+Bajo+","+Medio+","+Alto+","-Bajo-","-Medio-","-Alto-"))
  str(df.factorizado[,e])
  }
trans <- as(df.factorizado, "transactions")
summary(trans)