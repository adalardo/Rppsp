##############################
##' Functions to calculate subplot index and coordinates
##'
##' 'splitPlot' create a index for subplots of any dimension
##' 
##' 'subplotXY' calculate the x and y coordinates for subplots
##' 
##' 'xyMin' calculate x and y coordinates for a vector of suplots
##' 
##' @name subplot
##' @param dx x coordinate inside subplots
##' @param dy y coordinate inside subplots 
##' @param gx x coordinate for the all plot
##' @param gy y coordinate for the all plot 
##' @param sub.size size of the subplots
##' @param max.x maximum x coordinate
##' @param max.y maximum y coordinate  
##' @param plot.code subplot codes
##' @return return a new coordinates gx and gy or plot.code  with basal area and recalculated dbh for each tree. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' splitPlot(dx = runif(10, 0,20), dy = runif(10, 0, 20), split.x = 5)
##' xcode <-  LETTERS[1:16]
##' ycode <-  c(paste(0, 0:9, sep = ""), 10:15)
##' xyMin(subplot_code = paste(sample(xcode, size = 25, replace = TRUE), sample(ycode, size = 25, replace = TRUE), sep = ""))  
##' }
##' @rdname subplot
splitPlot <- function(dx, dy, split.x = 5, split.y = split.x, max.x = 20, max.y = max.x)
    {
        pos.x <- seq(split.x, max.x - split.x, by= split.x)
        pos.y <- seq(split.y, max.y - split.y, by= split.y)
        dx.ind <- apply(sapply(pos.x, function(x){ifelse(dx >= x, split.x,0)}), 1, sum)
        dy.ind <- apply(sapply(pos.y, function(x){ifelse(dy >= x, split.y,0)}), 1, sum)
        dxy <- paste(dx.ind, "x",dy.ind, sep="")
        dxy[grep("NA", dxy)] <- NA
        return(dxy) 
    }
##' @rdname subplot
subplotXY <- function(xcode = LETTERS[1:16], ycode= c(paste(0, 0:9, sep = ""), 10:15), xsub = 20, ysub = 20)
{
    nx <- length(xcode)
    ny <- length(ycode) 
    col <- rep(xcode, each = ny)
    lin <- rep(ycode, nx)
    subplot <- paste(col, lin, sep = "")
    xlim <- rep(seq(0, (nx - 1) * xsub, by = xsub), each = ny )
    ylim <- rep(seq(0, (ny - 1) * ysub, by = ysub), nx)
    return(data.frame(subplot, xlim, ylim))
}
##' @rdname subplot
xyMin <- function(subplot_code, xymin = subplotXY())
{
    seq.code <- match(subplot_code, xymin$subplot)
    data.frame(subplot_code, xmin = xymin$xlim[seq.code], ymin = xymin$ylim[seq.code])
}
