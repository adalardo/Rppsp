#################################################
#### FUNCTIONS TO CREATE CODES FOR SPECIES NAME
################################################
##create species code with the first 4 letters from genus and first 2 from epithet
## 'sp' is a vector with species name with or without "cf." "aff." - if has more than 3 letters will use first and last  to create codes. If there is duplicated codes, user are asked to enter a new code.
##' @export
spcode=function(sp)
{
genero=unlist(lapply(sp, function(x){strsplit(x," ")[[1]][1]}))
nomes<-strsplit(sp," ")
n.nomes=unlist(lapply(nomes, length))
epNA<-n.nomes==1
pos.epit<-cumsum(n.nomes)
unlist.nomes<-unlist(nomes)
epiteto=rep(0,length(sp))
epiteto<-unlist.nomes[pos.epit]
epiteto[epNA]<-NA
code=rep(NA, length(sp))
code[epNA]= paste(substr(genero[epNA],1,4),"00",sep="")
code[!epNA]= paste(substr(genero[!epNA],1,4),substr(epiteto[!epNA],1,2),sep="")
return(code)
}
##########################################################
# check if there is duplicated codes for different species
##########################################################
##' @export
lista.spcode=function(splist)
{
checar=unique(splist)
um.codigo=spcode(checar)
dupl=duplicated(um.codigo)
cod.n6<-(nchar(um.codigo))<6
code.sp=spcode(splist)
prob=dupl | cod.n6
n.prob=sum(prob)
## checking code size
 if(n.prob>0)
 {
 cod.prob<-um.codigo[prob]
 spnome.prob=checar[prob]
 novo.cod=rep(NA, n.prob)
 cat("\n Following species have duplicated codes or less than 6 letters code.\n\t Please, enter a new code. \n")
  for (i in 1:n.prob)
  {
   novo.cod<- readline(cat("NEW 6 LETTERS CODE ,",spnome.prob[i] , ",\n\t old code, ", cod.prob[i], ": "))
	code.sp[splist==spnome.prob[i]]<-novo.cod
	}
 }
return(code.sp)
}
######## teste###
#splist=rep(c("Ocotea pulchela","Alexandre", "ale", "Psichotria cf. nuda",  "Ocotea pulchra", "Amaioua guianensis", "Amaioua guidensis","Psichotria cfinA"), each=10)
#lista.spcode(splist)

