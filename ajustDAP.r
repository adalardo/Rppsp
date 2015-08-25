###########################################################################
## Funcao para calcular area basal e o dap corrigido para fustes multiplos
## Alexandre Adalardo de Oliveira, versao: 25 agosto 2015
##########################
#### DAP na base de dados
##########################
## fustes distintos de mesmo individuo separados por ";"
#########################################################
# modificando para formato longo
###  cada fuste uma linha
### DADOS TESTE  #
#dados<- data.frame(sp= rep(c("sp1", "sp2", "sp3"), 4), dbh01.mm= rep(c("110;12;130;14", "140;13;12", "14;13", "14"), 3), dbh02.mm= rep(c("150;16;17;18", "NA", "18", "18"), 3) , alt=seq(10, 50, len=12), date01="10-10-2002", date02="10-10-2005", status01= "A", status02 =sample(c(rep("A", 10), "D","D")), stringsAsFactors = FALSE, tag=paste(0, 1:12, sep="") )
adjDAP<- function(dados, dap= "dbh", censos= c("01","02"), sufixo= ".mm")
    {
        vrep <- Vectorize(rep.int, "times")
        namedbh=paste(dap, censos,sufixo, sep="")
        namedados <- names(dados)
        dbhano <- names(dados)[grep("dbh", names(dados))]
        nc <-substr(dbhano[! dbhano %in% namedbh], start=4, stop=5)
        for (k in nc)
            {
                npos <- grep(k, names(dados))
                dados <- dados[, - npos]
            }
        dbh =dados[,namedbh]
        dbh[dbh=="" | dbh==" " | is.null(dbh)]<-NA
        ncensos = length(censos)
        ntree = dim(dados)[1]
        nfuste <- matrix(NA, ncol=ncensos,nrow=ntree)
        if(ncensos>1)
            {
                ldbh0 <- apply(dbh, 2,strsplit, split=";")
                for(i in 1:ncensos)
                    {
                        nfuste[,i]<- sapply(ldbh0[[i]], length)
                    }
                maxfuste <- apply(nfuste, 1, max)
                diffuste <- maxfuste - nfuste
                difvf <- diffuste>0
                tdif<-apply(difvf>0, 1, sum)
            }else
                {
                    ldbh0 <- strsplit(dbh, split=";")
                    nfuste[,1]<-sapply(ldbh0, length)
                    maxfuste <- apply(nfuste, 1, max)
                    tdif<-rep(0, ntree)
                }
        datanew=NULL
        for(j in 1:ntree)
            {
                dtree<-dados[j, ]
                dtree$fuste <- 1
                nf = maxfuste[j]
                if(nf > 1)
                    {
                        if(tdif[j]> 0)
                            {
                                dfuste<- diffuste[j,]
                                indf <- dfuste > 0
                                fusteNA <- vrep(times= dfuste, ";NA")
                                #fusteNA <- vrep(times= c(0,2), "NA")
                                fusteNA <- sapply(fusteNA, paste, collapse="")
                                dbh[j,] <- paste(dbh[j,], fusteNA, sep="")
                            }
                        ndtree <- paste(rep("dtree", nf), collapse=", " )
                        strtree<-paste("dtree <- rbind(",ndtree,")", sep="")
                        eval(parse(text=strtree))
                        dtree$fuste = 1:nrow(dtree)
                     }
                dtree[,namedbh]<-NA
                if(ncensos>1)
                    {
                    newdbh <-unlist(sapply(dbh[j,],strsplit, split=";"))    
                    }
                else
                    {
                    newdbh <-unlist(sapply(dbh[j],strsplit, split=";"))     
                    }
                newdbh[newdbh=="NA"]<-NA
                dtree[,namedbh]<-as.numeric(newdbh)
                datanew=rbind(datanew,dtree)
                cat("Aguarde! Processando arvore ", j, "  de um total de ", ntree, "\n")   
            } 
        return(datanew)
    }
## debug(adjDAP)
## adjDAP(dados, censos=c("01", "02"))
#dados.fuste<- adjDAP(dados)
#################################################################################################
#### transforma dados no formato longo em formato de fuste unico com dap recalculado da soma das
#### areas basais de todos os fuste
######################################
fusteUnico <- function(dados.fuste, tag= "tag", dap= "dbh", censos= c("01","02"), sufixo= ".mm")
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
