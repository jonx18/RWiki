library('ROCR')
data<-df.big
set.seed(729375)#semilla para que sea reproducible
data$rgroup <- runif(dim(data)[[1]])#marco datos para subdividir
dTrainAll <- subset(data,rgroup<=0.9)#datos de entrenamiento
dTest <- subset(data,rgroup>0.9)#datos de test
outcomes=names(df.big)#nombres de columnas
#vars <- setdiff(colnames(dTrainAll),
#                c(outcomes,'rgroup'))
vars <- setdiff(colnames(dTrainAll),
                c('rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in%
                  c('factor','character')]#obtengo variables categoricas
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
                      c('numeric','integer')]#obtengo variables numericas
rm(data)#elimino informacion de mas
outcome <- 'infobox'
pos <- '1'
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0#preparo para dividir el trainingset
dCal <- subset(dTrainAll,useForCal)#set de calibracion
dTrain <- subset(dTrainAll,!useForCal)#set de entrenamiento reducido

#Pivote Table
table218 <- table(
  pagename=dTrain[,'pagename'],
  infobox=dTrain[,outcome],
  useNA='ifany')
print(table218)

#Predicciones de una variable para variables categoricas
mkPredC <- function(outCol,varCol,appCol) {
  pPos <- sum(outCol==pos)/length(outCol)#que tan seguido es positivo
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}
#aplicando a todas las variables categoricas (no hay ninguna)
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}
#luego de lo anterior se valida el AUC de las variables categoricas
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}
#Ahora si para variables numericas
mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,
                                     probs=seq(0, 1, 0.1),na.rm=T)))
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}
for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.55) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}