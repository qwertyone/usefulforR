##RQDA Code Key Table
x<-getCodingTable()
x<-x[c(2,4)]
x<-x[order(x[1]),]
tableCode<- x[match(unique(x$codename), x$codename),]
write.table(tableCode,file="ProjectCodeKey.txt",sep="|",col.names=TRUE,row.names=FALSE)
