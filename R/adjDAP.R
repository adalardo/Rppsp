##############################
##' Calculate dbh and basal area for multiple stem trees 
##'
##' @param dados  data frame with trees data.
##' @param dap  character string with the variable dhb name
##' @param censos vector with census code 
##' @param sufixo any suffix to add to dbh name (see exemple)
##' @return return a new data with basal area and recalculated dbh for each tree. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##'dados<- data.frame(sp= rep(c("sp1", "sp2", "sp3"), 4), dbh01.mm= rep(c("110;12;130;14", "140;13;12", "14;13", "14"), 3), dbh02.mm= rep(c("150;16;17;18", "NA", "18", "18"), 3) , alt=seq(10, 50, len=12), date01="10-10-2002", date02="10-10-2005", status01= "A", status02 =sample(c(rep("A", 10), "D","D")), stringsAsFactors = FALSE, tag=paste(0, 1:12, sep="") )
##' adjDAP(dados, dap = "dbh", censos = c("01", "02"), sufixo = ".mm")}
adjDAP <- function(dados, dap= "dbh", censos= c("01","02"), sufixo= ".mm")
    {
    library(tcltk)
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
        pb = tkProgressBar(title = "Separando dap por fuste", max = ntree)
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
                setTkProgressBar(pb, value = j, label = paste("arvore: ", j, " de um total de ", ntree , sep="")) 
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
                #cat("Aguarde! Processando arvore ", j, "  de um total de ", ntree, "\n")   
            } 
        return(datanew)
    }
