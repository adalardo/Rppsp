##############################################
##### get valid name from Flora do Brasil list
##############################################
##' @export
##' @importFrom flora get.taxa
floraName<-function(splist)
{
    gname <- flora::get.taxa(splist)
    res<-data.frame(validName = gname$scientif.name, shortName=splitname(gname$scientif.name, pattern=" ", indet=2 ))
    return(res)
}
############
## data list
############
##' @export
spListNames<-function(speciesCol)
    {
        splist <- unique(speciesCol)
        fname <- floraName(splist)
        fname
    }
