fusteUnico <-
function(dados, fuste = "stemtag", tag= "tag", dap= "dbh", censos= c("01","02"), sufixo= ".mm")
{
    namedbh=paste(dap, censos,sufixo, sep="")
    new_namedbh= paste(dap, censos,".cm", sep="")
    dap.cm <- aggregate(dados[,namedbh], list(tag=dados[,tag]), FUN= dap2cm)
    dap.cm[dap.cm==0]<-NA
    dados.cm<-dados[dados[,fuste] ==1, ]
    mtag<-match(dados.cm[,tag], dap.cm[,tag])
    dados.cm[,new_namedbh] <- dap.cm[mtag, namedbh]
    dados.cm <- dados.cm[, !(names(dados.cm) %in% c(namedbh, "fuste")) ]
    return(dados.cm)
}
####### calculate dap.cm from multistem dap.mm
dap2cm <- function(x)
    {
        ab <- sum(pi*(x/20)^2, na.rm=TRUE)
        round((4* ab/pi)^(1/2), 2)
    }
