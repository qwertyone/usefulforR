##RQDA Code Table
x<-getCodeTable()
x<-x[c(2,4)]
x<-x[order(x[1]),]
tableCode<- x[match(unique(x$cid), x$cid),]
write.table(tableCode,file="ProjectCodeKey.txt",sep="|",col.names=TRUE)
