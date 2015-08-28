fusteUnico <-
function(dados.fuste, tag= "tag", dap= "dbh", censos= c("01","02"), sufixo= ".mm")
{
    namedbh=paste(dap, censos,sufixo, sep="")
    abasal.cm <- aggregate(dados.fuste[, namedbh], list(tag=dados.fuste[,tag]), FUN= function(x){sum(pi*(x/20)^2, na.rm=TRUE)})
    abasal.cm[abasal.cm==0]<-NA
    abasal.cm[,namedbh] <-  round((4* abasal.cm[,namedbh]/pi)^(1/2), 2)
    namedbh.new= paste(dap, censos,".cm", sep="")
    dados.cm<-dados.fuste[dados.fuste$fuste==1, !(names(dados.fuste) %in% c(namedbh, "fuste")) ]
    dados.cm[, namedbh.new]<- abasal.cm[, -1]
    return(dados.cm)
}
