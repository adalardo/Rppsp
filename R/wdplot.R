WSGvalues <- function(PlotData=peic, Region=c("South America"), Region2= c("tropical", "extratropical"))
{
#peic<-read.csv("/home/ale/Ale2013/AleProjetos/Restinga/PP_Restinga/dadosUso/peic04_09FusteLinha.txt", sep="\t", header = TRUE, as.is=TRUE)
#str(peic)
require(doBy)
    ## #urlxls<-"https://datadryad.org/bitstream/handle/10255/dryad.235/GlobalWoodDensityDatabase.xls"
    ## wdglobal<-read.table("../data/wdglobal.csv", header=TRUE, as.is=TRUE, sep="\t")
    ## wdglobal$Region[wdglobal$Region=="South America (Tropical)"] <-"South America (tropical)"
## names(wdglobal)<-c("number","fam", "species", "woodDens", "region", "reference.number")
spgen<- strsplit(wdglobal$species, " ")
wdglobal$gen <- sapply(spgen, function(x){x[[1]]})
wdglobal$epit <- sapply(spgen, function(x){x[[2]]})
str(wdglobal)
wdglobal <- wdglobal[, c("number", "fam", "gen", "epit", "species", "woodDens", "region", "reference.number")]
## save(wdglobal, file="../data/wdglobal.rda")

load(file="../data/wdglobal.rda")
DRYADdata <- wdglobal
reg = unique(wdglobal$region)
  # DRYAD database Chave et al (2009)
### Selection of the Region of interest
regVF <- reg2VF <- rep(FALSE, dim(DRYADdata)[1])
for(i in Region)
    {
      regVF <- regVF | grepl(i, DRYADdata$region)    
  }
if(! is.null (Region2))
    {
        for(j in Region2)
            {
                reg2VF <- reg2VF | grepl(j, DRYADdata$region)
            }
    } 
SubDRYADdata <- DRYADdata[regVF & reg2VF,]
print(paste("DRYAD data stored",nrow(SubDRYADdata),"wood density measures in your region of interest"))
## Attributing a WSG value to each taxa of the plot
Taxa<-summaryBy(data~fam+gen+epit, data=PlotData)[,-c(4)]  # List of all taxa stored in Plotdata
  if(nrow(Taxa)<1) stop("Can not detect the number of taxa, please check if your table contains family ('Family'), genus ('Genus') and species ('Species') columns")
  print(paste("Your table contains",nrow(Taxa),"taxa"))
  WSG<-vector(mode="numeric",length=nrow(Taxa))          # Vector for Wood Specific Gravity measures
  levelWSG=vector(mode="character",length=nrow(Taxa))    # Vector indicating  at which taxonomical level WSG values are attributed
  for (i in 1:length(WSG)){
    tmp=SubDRYADdata[SubDRYADdata$gen%in%Taxa$gen[i] & SubDRYADdata$epit%in%Taxa$epit[i],] # Mean WSG at the species level
    if(nrow(tmp)>0){
      WSG[i]=mean(tmp$woodDens)
      levelWSG[i]="Species"
    } else{
      tmp=SubDRYADdata[SubDRYADdata$gen%in%Taxa$gen[i],]  # Mean WSG at the genus level
      if(nrow(tmp)>0){
        WSG[i]=mean(tmp$woodDens)
        levelWSG[i]="Genus"
      } else{
        tmp=SubDRYADdata[SubDRYADdata$fam%in%Taxa$fam[i],]  # Mean WSG at the family level
        if(nrow(tmp)>0){
          WSG[i]=mean(tmp$woodDens)
          levelWSG[i]="Family"
        } else{
          WSG[i]=NA           # Mean WSG in the plot
          levelWSG[i]="Plot mean"
        }
      }
    }
  } 
  WSG[levelWSG=="Plot mean"]=mean(WSG[levelWSG!="Plot mean"])
  cbind(Taxa,WSG,levelWSG)
  #Result=merge(PlotData,WSGvalues, by=c("fam","gen","epit")) 
  #if(nrow(Result)!=nrow(PlotData)) warning(paste("The input and the output tables have a different number of rows"))
  #return(Result)
}

wd2plotdata <- function(plotdata, wsgdata)
    {
        merge(plotdata, wd, by=c("fam", "gen", "epit"))
    }
