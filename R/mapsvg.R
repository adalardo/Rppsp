############################################
### Alexandre Adalardo 15 de outubro de 2018
#############################################
# permanent plot maps
#####################
##' @title Graphic of trees maped 
##' @description Map of trees from permanent plot based on cartesian coordinates 
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
##' @examples
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' mapsvg(dataplot, save.svg =FALSE)
##' }
##' 
##' @export subquad
##'
subquad <- function(dx, dy, size.split = 5, max.size = 20)
    {
        pos <- seq(size.split, max.size - size.split, by= size.split)
        codx <- cody  <-rep(0, length(dx))
        ## for(i in pos){
        ##     codx <- codx + (dx > i)
        ##     cody <- cody + (dy > i)
        ## }
        dx.ind <- apply(sapply(pos, function(x){ifelse(dx >= x, size.split, 0)}), 1, sum)
        dy.ind <- apply(sapply(pos, function(x){ifelse(dy >= x, size.split, 0)}), 1, sum)
        paste(dx.ind, "x",dy.ind, sep="")
    }
################################
index.map<- function(dx, dy, size.split = 5, max.size = 20)
{
    pos <- seq(size.split, max.size - size.split, by= size.split)
    dy.ind <- apply(sapply(pos, function(x){ifelse(dy >= x, 1,0)}), 1, sum)
    dx.ind <- rep(NA, length(dy.ind))
    max.ind = 0
    for(i in sort(unique(dy.ind)))
    {
        if(i%%2 == 0)
        {
            dx.ind[dy.ind==i] <- max.ind + rank(dx[dy.ind==i])
        }
        if(i%%2 != 0)
        {
            dx.ind[dy.ind==i] <- max.ind + rank(1/(dx[dy.ind==i]))
        }
     max.ind= max(dx.ind, na.rm=TRUE)   
    }
    return(dx.ind)
}
#################################
mapsvg <- function( censo = peic09, quad = "A00", size = 5, max.size=20, save.svg = TRUE, wd = getwd(), dx = "dx", dy = "dy",  tag = "tag", dap = "dap", status= NULL, mapsize = c(13,13))
{
    options(warn = -1)
    library("grid")
    library("gridSVG")
    dataquad <- censo[censo$quad == quad,]
    qind <- subquad(dataquad[, dx], dataquad[,dy], size.split = size, max.size = max.size)
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
        is.na(dbh0) <- 5 # small size dbh for trees without dbh info 
##############################
## plot here
##############################
        dev.new(width = mapsize[1], height = mapsize[2])
        vptop<- viewport(y=0.9, width=0.8, height=0.2)
        grid.text(x=0.5, y=0.9, paste("Parcela ", quad,"  - subparcela", subq[j]) ,vp= vptop, gp=gpar(fontsize = 20))
        vp <- viewport(width = 0.8, height = 0.8, xscale=c(xys[1],xys[1]+size), yscale=c(xys[2], xys[2]+size))
        pushViewport(vp)
        grid.rect(gp = gpar(col = "black"))
        grid.xaxis(seq(0,size, by = size/5), at=seq(xys[1],xys[1]+size,by = size/5), gp=gpar(fontsize=15))
        grid.yaxis(seq(0,size, by = size/5), at=seq(xys[2],xys[2]+size, by = size/5), gp=gpar(fontsize=15))
        for(i in 1:nrow(subquad))
        {
            grid.circle(x= subquad[i, dx],y=subquad[i, dy], r= log(dbh0[i])/20, default.units="native", gp=gpar(fill=ifelse(subquad[i, status]=="A" | subquad[i, status]=="AS" ,rgb(0,1,0, 0.5),rgb(1,0,0,0.5)), col="black"), name = arv_key[i])
            grid.text(paste(subquad[i, tag]), x= subquad[i, dx]+log(dbh0[i])/20 ,y=subquad[i, dy]+log(dbh0[i])/15, default.units="native", gp = gpar(cex = 2))
        }
        grid.text(c(xys[1], xys[1]+ size) ,x=  c(0,1), y= c(-0.08, -0.08),  gp = gpar(cex=2.5, col="red"))
        grid.text(c(xys[2], xys[2]+ size) ,y=  c(0,1), x= c(-0.08, -0.08),  gp = gpar(cex=2.5, col = "red"))
        #grid.circle(x= a00_0x0$dx,y=a00_0x0$dy, r= log(dbh0)/20, default.units="native", gp=gpar(fill=c(rgb(0,1,0, 0.5),rgb(1,0,0,0.5))[as.factor(a00_0x0$status=="A")], col="gray")) ## this is a much more efficienty way to run the loop above but in order to svg ids write need it is necessary a for expression
        if(save.svg)
        {
           ## grid.export(file.path(wd, paste("map", size, "_",quad,"quad_",subq[j],".svg",sep="")) , uniqueNames=FALSE)
            grid.export(file.path(wd, paste("map", quad,"quad_",subq[j],".svg",sep="")) , uniqueNames=FALSE)
        }
#######################################
# plot end
#######################################        
    }
}
##############################
# audit  plot maps
##############################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param audit auditory data frame from permanent plot tree censo data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
#' @examples
##' 
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' auditsvg(dataplot, save.svg =FALSE)
##' }
##' 
##'
##'
##' 
########################################
auditsvg <- function(audit, quad = "A00", save.svg = TRUE, wd = getwd(), dx = "new_dx2018", dy = "new_dy2018",  tag = "num_tag", dap = "dap2018", error = "errorType", mapsize = c(13,13))
{
    options(warn = -1)
    library("grid")
    library("gridSVG")
    dataquad <- audit[audit$quadrat == quad,]
    xyna <- is.na(dataquad[,dx]) | is.na(dataquad[,dy])
    xy <- dataquad[, c(dx, dy)]
    xy[xyna, ] <- dataquad[xyna,c("old_dx", "old_dy")]
    dbh0 <- dataquad[,dap]
    dbh0[is.na(dbh0)] <- 10 
    arv_key <- paste("arv_", dataquad[,tag], sep="")
    tipo <- as.factor(dataquad[, error])
    utipo <- levels(tipo)
    ntipo <- length(utipo)
###########################
## plot here
###########################
    dev.new(width = mapsize[1], height = mapsize[2])
    vptop<- viewport(y=0.9, width=0.8, height=0.2)
    grid.text(x=0.5, y=0.9, paste(" Auditoria Parcela ", quad) ,vp= vptop, gp=gpar(fontsize = 20))
    vp <- viewport(width = 0.8, height = 0.8, xscale=c(0, 20), yscale=c(0, 20))
    pushViewport(vp)
    grid.rect(gp = gpar(col = "black"))
    grid.xaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    grid.yaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    cols <- c(rgb(0,0,0, 0.3), rgb(0,1,0, 0.5), rgb(0,0,1, 0.5), rgb(1,1,0, 0.5), rgb(1,0,1, 0.5), rgb(0,1,1, 0.5))
    int <- 1
        for(i in 1:nrow(dataquad))
        {
            grid.circle(x= xy[i, 1],y=xy[i, 2], r= log(dbh0[i])/10, default.units="native", gp=gpar(fill= cols[tipo[i]], col="black"), name = arv_key[i])
            grid.text(paste(dataquad[i, tag]), x= xy[i, 1] + (log(dbh0[i])/8) ,y = xy[i, 2] + (int *log(dbh0[i])/8), default.units="native", gp = gpar(cex = 1.2))
            int = int * -1
        }
    grid.circle(x= c(1.5, 6.5, 11.5, 16.5)[1:ntipo],y =-1.7, r= 0.3, default.units="native", gp=gpar(fill= cols[1:ntipo], col="black"))
    grid.text(utipo[1:ntipo], x = (c(1.5, 6.5, 11.5, 16.5)+ .5)[1:ntipo]   , y= -1.7,  gp = gpar(cex = 1.2), , default.units="native", just= "left")
        ## grid.text(c(xys[2], xys[2]+ 5) ,y=  c(0,1), x= c(-0.08, -0.08),  gp = gpar(cex=2.5, col = "red"))
        #grid.circle(x= a00_0x0$dx,y=a00_0x0$dy, r= log(dbh0)/20, default.units="native", gp=gpar(fill=c(rgb(0,1,0, 0.5),rgb(1,0,0,0.5))[as.factor(a00_0x0$status=="A")], col="gray")) ## this is a much more efficienty way to run the loop above but in order to svg ids write need it is necessary a for expression
        if(save.svg)
        {
            grid.export(file.path(wd, paste("auditmap",quad,".svg",sep="")) , uniqueNames=FALSE)
        }
#######################################
# plot end
#######################################        
}
##############################
# audit  plot maps
##############################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param audit auditory data frame from permanent plot tree censo data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##'
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' auditsvg(dataplot, save.svg =FALSE)
##' }
##' 
##'
##'
##' 
########################################
ordersvg <- function(audit, quad = "A00", save.svg = TRUE, wd = getwd(), dx = "new_dx2018", dy = "new_dy2018",  tag = "num_tag", dap = "dap2018", error = "errorType", mapsize = c(13,13))
{
    options(warn = -1)
    library("grid")
    library("gridSVG")
    dataquad <- audit[audit$quadrat == quad,]
    xyna <- is.na(dataquad[,dx]) | is.na(dataquad[,dy])
    xy <- dataquad[, c(dx, dy)]
    xy[xyna, ] <- dataquad[xyna,c("old_dx", "old_dy")]
    dbh0 <- dataquad[,dap]
    dbh0[is.na(dbh0)] <- 10 
    arv_key <- paste("arv_", dataquad[,tag], sep="")
    tipo <- as.factor(dataquad[, error])
    utipo <- levels(tipo)
    ntipo <- length(utipo)
    idmap <- index.map(dx=xy[,dx], dy=xy[,dy])
    xy <- xy[order(idmap),]
    narv <- nrow(xy)
###########################
## plot here
###########################
    dev.new(width = mapsize[1], height = mapsize[2])
    vptop<- viewport(y=0.9, width=0.8, height=0.2)
    grid.text(x=0.5, y=0.9, paste(" Auditoria Parcela ", quad) ,vp= vptop, gp=gpar(fontsize = 20))
    vp <- viewport(width = 0.8, height = 0.8, xscale=c(0, 20), yscale=c(0, 20))
    pushViewport(vp)
    grid.rect(gp = gpar(col = "black"))
    grid.xaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    grid.yaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    cols <- c(rgb(0,0,0, 0.3), rgb(0,1,0, 0.5), rgb(0,0,1, 0.5), rgb(1,1,0, 0.5), rgb(1,0,1, 0.5), rgb(0,1,1, 0.5))
     int <- 1
        for(i in 1:narv)
        {
            if(i < narv)
            {
                grid.lines(x = c(xy[i,1], xy[i+1, 1]), y = c(xy[i,2], xy[i+1, 2]), default.units="native", gp= gpar(col=rgb(0,0,0,.2), lwd=3, lty=2))
            }
            grid.circle(x= xy[i, 1],y=xy[i, 2], r= log(dbh0[i])/10, default.units="native", gp=gpar(fill= cols[tipo[i]], col="black"), name = arv_key[i])
            grid.text(paste(dataquad[i, tag]), x= xy[i, 1] + (log(dbh0[i])/8) ,y = xy[i, 2] + (int *log(dbh0[i])/8), default.units="native", gp = gpar(cex = 1.2))
            grid.text(as.character(i), x= xy[i, 1] ,y = xy[i, 2], default.units="native", gp = gpar(cex = 1.5, col=rgb(1,1,1)))
            int = int * -1
        }
    grid.circle(x= c(1.5, 6.5, 11.5, 16.5)[1:ntipo],y =-1.7, r= 0.3, default.units="native", gp=gpar(fill= cols[1:ntipo], col="black"))
    grid.text(utipo[1:ntipo], x = (c(1.5, 6.5, 11.5, 16.5)+ .5)[1:ntipo]   , y= -1.7,  gp = gpar(cex = 1.2), , default.units="native", just= "left")
        ## grid.text(c(xys[2], xys[2]+ 5) ,y=  c(0,1), x= c(-0.08, -0.08),  gp = gpar(cex=2.5, col = "red"))
        #grid.circle(x= a00_0x0$dx,y=a00_0x0$dy, r= log(dbh0)/20, default.units="native", gp=gpar(fill=c(rgb(0,1,0, 0.5),rgb(1,0,0,0.5))[as.factor(a00_0x0$status=="A")], col="gray")) ## this is a much more efficienty way to run the loop above but in order to svg ids write need it is necessary a for expression
        if(save.svg)
        {
            grid.export(file.path(wd, paste("ordermap",quad,".svg",sep="")) , uniqueNames=FALSE)
        }
#######################################
# plot end
#######################################        
}
## teste
## audit <- read.table("/home/aao/Ale2016/AleProjetos/PPPeic/censo2018/auditDataAll/auditData15Jan2019/treeaudit.csv", header = TRUE, as.is=TRUE, sep=",")
## audit$new_dx2018[is.na(audit$new_dx201)] <- audit$old_dx[is.na(audit$new_dx2018)]
## audit$new_dy2018[is.na(audit$new_dy201)] <- audit$old_dy[is.na(audit$new_dy2018)]
## #indpos <- index.map(dx = audit$new_dx2018, dy= audit$new_dy2018)
## audit <- audit[!is.na(audit$num_tag),]
## quad = "B11"; save.svg = TRUE; dx = "new_dx2018"; dy = "new_dy2018";  tag = "num_tag"; dap = "dap2018"; error = "errorType"; mapsize = c(13,13)
