adjDAP <-
function(dados, dap= "dbh", censos= c("01","02"), sufixo= ".mm")
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
