##############################################
##### get valid name from Flora do Brasil list
##############################################
flora.name<-function(splist)
{
    require(flora)
    gname <- get.taxa(splist)
    res<-data.frame(validName = gname$scientif.name, shortName=splitname(gname$scientif.name, pattern=" ", indet=2 ))
    return(res)
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
