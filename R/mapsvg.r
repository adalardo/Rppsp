############################################
### Alexandre Adalardo 15 de outubro de 2018
#############################################
# permanent plot maps
#####################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param size.split the size of the subquadrat to map in each graphic.
##' @param censo data frame from permanent plot tree censo cartezian data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##' @references 
##' @keywords
##' @examples
##' 
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' mapsvg(dataplot, save.svg =FALSE)
##' }
##' 
##' @export subquad
##'
##' 
subquad <- function(dx, dy, size.split = 5, max.size = 20)
    {
        pos <- seq(5,max.size - size.split, by= size.split)
        codx <- cody  <-rep(0, length(dx))
        ## for(i in pos){
        ##     codx <- codx + (dx > i)
        ##     cody <- cody + (dy > i)
        ## }
        dx.ind <- apply(sapply(pos, function(x){ifelse(dx >= x, 5,0)}), 1, sum)
        dy.ind <- apply(sapply(pos, function(x){ifelse(dy >= x, 5,0)}), 1, sum)
        paste(dx.ind, "x",dy.ind, sep="")
    }
################################
mapsvg <- function( censo = peic09, quad = "A00", size = 5, max.size=20, save.svg = TRUE, wd = getwd(), dx = "dx", dy = "dy",  tag = "tag", dap = "dap", status= NULL)
{
    options(warn = -1)
    library("grid")
    library("gridSVG")
    dataquad <- censo[censo$quad == quad,]
    qind <- subquad(dataquad[, "dx"], dataquad[,"dy"], size.split = size, max.size = max.size)
    subq <- sort(unique(qind))
    for(j in 1: length(subq))
    {
        subquad <- dataquad[qind == subq[j],]
        xys <-  as.numeric(strsplit(subq[j], "x")[[1]])
        arv_key <- paste("arv_", subquad[,tag], sep="")
        if(is.character(censo$dap))
        {
            dbh0 <- as.numeric(sapply(strsplit(subquad[,dap], ";"), function(x){x[1]}))
        }else{dbh0 <- subquad[,dap]}
##############################
## plot here
##############################
        dev.new(width=13, height=13)
        vptop<- viewport(y=0.9, width=0.8, height=0.2)
        grid.text(x=0.5, y=0.9, paste("Parcela ", quad,"  - subparcela", subq[j]) ,vp= vptop, gp=gpar(fontsize = 20))
        vp <- viewport(width = 0.8, height = 0.8, xscale=c(xys[1],xys[1]+5), yscale=c(xys[2], xys[2]+5))
        pushViewport(vp)
        grid.rect(gp = gpar(col = "black"))
        grid.xaxis( 0:5, at=seq(xys[1],xys[1]+5,by=1), gp=gpar(fontsize=15))
        grid.yaxis(0:5, at=seq(xys[2],xys[2]+5,by=1), gp=gpar(fontsize=15))
        for(i in 1:nrow(subquad))
        {
            grid.circle(x= subquad[i, dx],y=subquad[i, dy], r= log(dbh0[i])/20, default.units="native", gp=gpar(fill=ifelse(subquad[i, status]=="A",rgb(0,1,0, 0.5),rgb(1,0,0,0.5)), col="black"), name = arv_key[i])
            grid.text(paste(subquad[i, tag]), x= subquad[i, dx]+log(dbh0[i])/20 ,y=subquad[i, dy]+log(dbh0[i])/15, default.units="native", gp = gpar(cex = 2))
        }
        grid.text(c(xys[1], xys[1]+5) ,x=  c(0,1), y= c(-0.08, -0.08),  gp = gpar(cex=2.5, col="red"))
        grid.text(c(xys[2], xys[2]+ 5) ,y=  c(0,1), x= c(-0.08, -0.08),  gp = gpar(cex=2.5, col = "red"))
        #grid.circle(x= a00_0x0$dx,y=a00_0x0$dy, r= log(dbh0)/20, default.units="native", gp=gpar(fill=c(rgb(0,1,0, 0.5),rgb(1,0,0,0.5))[as.factor(a00_0x0$status=="A")], col="gray")) ## this is a much more efficienty way to run the loop above but in order to svg ids write need it is necessary a for expression
        if(save.svg)
        {
            grid.export(file.path(wd, paste("map",quad,"quad_",subq[j],".svg",sep="")) , uniqueNames=FALSE)
        }
#######################################
# plot end
#######################################        
    }
}
