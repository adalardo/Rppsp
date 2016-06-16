###################################
## split species name using pattern
####################################
splitname <- function(splist, pattern=" ", indet=2)
{
    res <-matrix(NA, nrow=length(splist), ncol=2)
    lsn <- strsplit(splist, split= pattern)
    lenname <- unlist(lapply(lsn,length))
    for(i in 1:length(lennames))
        {
            res[i,1]<- lsn[[i]][1]
            if(lennames[i]>indet)
               {
                   res[i,2]<- lsn[[i]][2]
               }else
                   {
                       res[i,2]<-  "sp."
                   }
        }
}
##############################################
##### get valid name from Flora do Brasil list
##############################################
flora.name<-function(splist)
{
    require(flora)
    gname <- get.taxa(splist)
    res<-data.frame(validName = gname$scientif.name, shortName=splitname(gname$scientif.name, pattern=" ", indet=2 ))  
}
############
## data list
############
datasplist<-function(speciesCol)
    {
        splist <- unique(speciesCol)
        fname <- flora.name(splist)
        fname
    }
