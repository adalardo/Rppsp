#pp###########################################
### Rppsp -  Neighbours and Point Pattern ##
############################################
##' @title
##' Neighbours Functions and Spatial Point Patterns
##'
##' @description
##' Functions to analysis population spatial arrangement and respective graphics
##'
##' @details
##' A set of functions to analysis point patterns process based on K-Ripley statistics for univariate and bivariate type of points. Originally these functions were coded for a study biological populations of sessils organisms, but can be apply for any cartesian mapping objects. Thoses functions are first coded for EcoVirtual Project  \url{http://ecovirtual.ib.usp.br}.
##' 
##' `neighbors` identifies neighbors around all points in a cartesian coordinate space.
##' `countNeighbors` counts the number of neighbors for each point for a given r distance.
##' 
##' `edgeData` torus border data adjustment.
##' 
##' `meanNeighbors`  calculate the mean number of neighbors for a sequence of r distances.
##' 
##' `meanNeighborsBi`  calculate the mean number of neighbors for a sequence of r distances for a bivariate points. First code represents the target points and second level code the neighbors points.
##' 
##' `ppStats` calculate K-Ripley, L-Ripley and O-Ring statistics for univariate points in a cartesian coordinate space for a sequence of r distances.
##'
##' `ppStatsBi` calculate K-Ripley, L-Ripley and O-Ring statistics for bivariate points in a cartesian coordinate space for a sequence of r distances.
##'
##' `minContrast`  calculate mininum contrasts for estimate parameters for cluster Thomas Process.
##' 
##' `estimaThomas` estimates the Thomas process function parameters
##' 
##' `envelopeSim` calculates Monte Carlo confidence envelops for complete spatial random or cluster Thomas process.
##' 
##' `envelopeSimBi` calculates Monte Carlo confidence envelops for complete spatial random bivariate point process.
##' 
##' @name pointPattern
##' @aliases xyTest limTest estimaThomas minContrast
##' @param x,y Numeric vectors of cartesian coordinates of neighbor points.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane. 
##' @param xc,yc Numeric scalar coordinates of focus point.
##' @param r Numeric neighborhood radius.
##' @param xyStat Point pattern statistics dataframe.
##' @param par Cluster Thomas process parameters: $\kappa$, $\sigma$. 
##' @param parIni Initial parameters used by [optim()] to estimate Thomas Process parameters.
##' @return The functions return graphics with the simulation results, and a
##' matrix with the population size for deterministic and stochastic models.
##' @author Alexandre Adalardo de Oliveira \email{ecovirtualpackage@gmail.com}
##' @seealso \code{\link{metaComp}}, \url{http://ecovirtual.ib.usp.br}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords population dynamics simulation
###################
## basic functions
###################
## test x y
###########
##' @rdname pointPattern
##' @export
xyTest <- function(x, y)
{
    
    if(length(x) != length(y))
    {
        stop("'x' and 'y' should have the same length")
    }
    if(!all(is.numeric(x) | is.integer(x),  is.numeric(y) | is.integer(y)))
    {
        stop("'x' and 'y' should have only numeric or integer values")
    }
}
################################
## test if x y is out of border
################################
##' @rdname pointPattern
##' @export
limTest <- function(x, y, xlim, ylim)
{
    if(any(x > xlim[2] | x < xlim[1] | y > ylim[2] | y < ylim[1]))
    {
        stop("Some points are out of border define in 'xlim' and 'ylim'")
    }
}
########################
##' @title
##' Edge Data Correction
##'
##' @description
##' Adjust border effect using a torus correction connecting vertical and horizontal borders for a continuous spaces 
##' 
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax Maximum distance for neighbors spatial statistics.
##' @return `edgeData` returns a list with tree positions:  
##' `edgeData` data with border torus correction coordinates
##' `originalData` original data 'x' and 'y' coordinates
##' `pointsVF` logical vector with the same length of 'edgeData' indicating the 'TRUE' data coordinates and 'FALSE' border adjustment coordinates.
##' @author Alexandre Adalardo de Oliveira \email{ecovirtualpackage@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords border effect correction
##' @examples 
##' \dontrun{
##' edgeData(x = runif(10, 0, 1), y= runif(10, 0, 1), xlim = c(0,1), ylim = c(0,1), rMax = 0.25)
##' }
##' @export
edgeData <- function(x, y, xlim = c(0,1), ylim = c(0,1), rMax = min(c(xlim[2], ylim[2]))/4)
{
    xyTest(x = x, y = y)
    limTest(x = x, y = y, xlim = xlim, ylim = ylim)
    x0 <- x - xlim[1]
    y0 <- y - ylim[1]
    xMax <- diff(xlim) 
    yMax <- diff(ylim)
    yUp <- y[y <= rMax] + yMax
    xUp <- x[y <= rMax]
    yDown <- y[y >= (yMax - rMax)] - yMax
    xDown <- x[y >= (yMax - rMax)]
    y1 <- c(yDown, y, yUp)
    x1 <- c(xDown, x, xUp)
    xLeft <- x1[x1 >= (xMax - rMax)] - xMax
    yLeft <- y1[x1 >= (xMax - rMax)]
    xRight <- x1[x1 <= rMax] + xMax
    yRight <- y1[x1 <= rMax]
    list(edgeData = data.frame(xEdge = c(xLeft, x1, xRight), yEdge =  c(yLeft, y1, yRight)), originalData = data.frame(x = x0, y= y0), pointsVF = rep(c(FALSE, FALSE, TRUE, FALSE, FALSE), times = c(length(xLeft), length(xDown), length(x), length(xUp), length(xRight))) )
}
##########################
##' @title
##' Count neighbors around 
##'
##' @description
##' Count number of neighbor for each point for a given r distance neighborhood
##' 
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param r dis.tance for neighborhood definition.
##' @return 'countNeighbors' returns a integer vector  with counts the number of neighbors around each point at the maximum distance 'r'
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references
##' Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' 
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##'
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' countNeighbors(x = runif(100, 0, 100), y = runif(100, 0, 100), xlim= c(0,100), ylim = c(0,100), r = 10)
##' }
##' 
##' @export
countNeighbors <- function(x, y, xlim = c(0,1), ylim = c(0,1), r = 0.1)
{
    nP <-  length(x)
    xyEdge <- edgeData(x, y, xlim, ylim, rMax = r)

    distMat <- as.matrix(dist(cbind(xyEdge$edgeData$xEdge, xyEdge$edgeData$yEdge)))
    distXY <- distMat[xyEdge$pointsVF,]
    apply(distXY <= r , 1, sum) -1
}
#############
## Neighbors
#############
##########################################
##' @title
##' Identifies neighbors around each point 
##'
##' @description
##' Identifies neighbors for each points in a given r distance neighborhood returning the index of neighbor points.
##' 
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param r distance that define neighborhood limits.
##' @return 'neighbors' returns a list of integer vector. Each vector include indexs of neighbors around the point at the maximum distance 'r'. Vector position in the list represents the target point position in the vectors 'x' and 'y'.
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' neighbors(x = runif(100, 0, 100), y = runif(100, 0, 100), r = 10)
##' }
##' 
##' @export
neighbors <- function(x, y, r)
{
    xyTest(x = x, y = y)
    distMat <- as.matrix(dist(cbind(x, y)))
    apply(distMat <= r , 1, which)
}
#############################################################
##' @title
##' Mean number of neighbors for different neighborhood sizes
##'
##' @description
##' Calculate mean number of neighbors as a function of distances from each point 
##'
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax maximum distance for neighborhood definition.
##' @param step increase distance size for neighborhood.
##' @return 'meanNeighbors' returns a data frame with two numeric vectors. The first 'r' represents the neighborhood distance definition; 'meanNeighbors' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' meanNeighbors(x = runif(100, 0, 1), y = runif(100, 0, 1))
##' }
##' @export
meanNeighbors <- function(x, y, xlim = c(0,1), ylim = c(0,1), rMax = min(c(xlim[2], ylim[2]))/4, step = rMax/100)
{
    xyEdge <- edgeData(x, y, xlim = xlim, ylim = ylim, rMax = rMax)
    rSeq <- seq(step, rMax, by = step)
    distMat <- as.matrix(dist(cbind(xyEdge$edgeData$xEdge, xyEdge$edgeData$yEdge)))
    distXY <- distMat[xyEdge$pointsVF,]
    nN <- sapply(rSeq, function(x){mean(apply(distXY <= x , 1, sum))})
    return(data.frame(r = rSeq, meanNeighbors = nN - 1))
}
#################################################
### MEAN NEIGHBOR BIVARIATE
## calculate the mean number of "sp2" around "sp1"
#################################################
##' @title
##' Mean number of neighbors for different neighborhood sizes for a bivariate points
##'
##' @description
##' Calculate mean number of second level factor points as a function of the distances from the first level factor points.
##'
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param code factor vector with two levels and same length as 'x' and 'y'
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax maximum distance for neighborhood definition.
##' @param step increase distance size for neighborhood.
##' @param ways type of bivariate calculation: 'order' uses first levels as target points; 'reverse' uses second level as target points; 'both' calculate the two bivariates means.
##' @return 'meanNeighborsBi' returns a data frame with numeric vectors. The first 'r' represents the neighborhood distance definition; 'meanNeighbors_12' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' meanNeighbors(x = runif(100, 0, 1), y = runif(100, 0, 1))
##' }
##' @export
meanNeighborsBi <- function(x, y, code, xlim = c(0,1), ylim = c(0,1), rMax = min(c(xlim[2], ylim[2]))/4, step = rMax/100, ways = c("order", "reverse", "both"))
{
    ways <- match.arg(ways)
    spp <- unique(code)
    if(length(spp) != 2)
    {
        stop("Need to have two codes in code vector")
    }
    if(ways == "reverse")
    {
        spp <- rev(spp)
    }
    sp1VF <- code == spp[1] 
    xyEdge02 <- edgeData(x[!sp1VF], y[!sp1VF], xlim = xlim, ylim = ylim, rMax = rMax)
    rSeq <- seq(step, rMax, by = step)
    xy01 <- data.frame(x = c(x[sp1VF], xyEdge02$edgeData$xEdge), y = c(y[sp1VF], xyEdge02$edgeData$yEdge))
    distMat01 <- as.matrix(dist(xy01))
    distXY01 <- distMat01[1: sum(sp1VF), (sum(sp1VF) + 1) : ncol(distMat01)]
    nN_12 <- sapply(rSeq, function(x){mean(apply(distXY01 <= x , 1, sum))})
    res <- data.frame(r = rSeq, meanNeighbors_12 = nN_12)
    if(ways == "both")
    {
    xyEdge01 <- edgeData(x[sp1VF], y[sp1VF], xlim = xlim, ylim = ylim, rMax = rMax)
    xy02 <- data.frame(x = c(x[!sp1VF], xyEdge01$edgeData$xEdge), y = c(y[!sp1VF], xyEdge01$edgeData$yEdge))
    distMat02 <- as.matrix(dist(xy02))
    distXY02 <- distMat02[1: sum(!sp1VF), (sum(!sp1VF) + 1):ncol(distMat02)]
    nN_21 <- sapply(rSeq, function(x){mean(apply(distXY02 <= x , 1, sum))})
    res$meanNeighbors_21 <-  nN_21
    }
    return(res)
}
##############################################################
## Point Patterns Stats: K-Ripley, L-Ripley, O-Ring  estimates
##############################################################
#################################################
##' @title
##' Point Pattern Statistics Calculation
##'
##' @description
##' Compute point pattern multiscale statistics K-Ripley, L-Ripley and O-Ring. K-Ring is a statistics based on mean number of neighbors around each point in a function of the neighbourhood distances divide by the intensity of points $\lambda$. The expected K-Ripley for complete random distribution is equal $pi * r^2$. L-Ripley is define here as $\sqrt{K_{(r)}/\pi} - r$ an parametrization to have expectance equal zero. O-Ring calculate L-Ripley for a ring of width equal to `step` argument. 
##'
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax maximum distance for neighborhood definition.
##' @param step increase distance size for neighborhood.
##' @param ways type of bivariate calculation: 'order' uses first levels as target points; 'reverse' uses second level as target points; 'both' calculate the two bivariates means.
##' @return 'ppStats' returns a data frame with numeric vectors. The first 'r' represents the neighborhood distance definition; '' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' ppStats(x = runif(100), y = runif(100), xlim = c(0,1), ylim = c(0,1), rMax = 0.25, step = 0.02)
##' }
##' @export
ppStats <- function(x, y, xlim = c(0,1), ylim = c(0,1), rMax = min(c(xlim[2], ylim[2]))/4, step = rMax/100)
{
    mN <- meanNeighbors(x = x, y = y, xlim= xlim, ylim= ylim, rMax = rMax, step = step)
    mN$K_Obs <- mN$meanNeighbors * (diff(xlim)* diff(ylim))/length(x)
    mN$L_Obs <- sqrt(mN$K_Obs/pi) - mN$r
    mN$O_Obs <- mN$L_Obs - c(0, mN$L_Obs[- (length(mN$L_Obs))])
    return(mN)
}
##########################################
### Statistics for Bivariate point pattern
##########################################
##' @title
##' Bivariate Point Pattern Statistics Calculation
##'
##' @description
##' Compute point pattern multiscale statistics K-Ripley, L-Ripley and O-Ring for a bivariate point pattern. K-Ring is a statistics based on mean number of neighbors of type 2 point around each point of type 1 in a function of the neighbourhood distances divide by the intensity of points $\lambda$. The expected K-Ripley for complete random distribution is equal $pi * r^2$. L-Ripley is define here as $\sqrt{K_{(r)}/\pi} - r$ an parametrization to have expectance equal zero. O-Ring calculate L-Ripley for a ring of width equal to `step` argument. 
##'
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param code factor vector with two levels and same length as 'x' and 'y'
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax maximum distance for neighborhood definition.
##' @param step increase distance size for neighborhood.
##' @param ways type of bivariate calculation: 'order' uses first levels as target points; 'reverse' uses second level as target points; 'both' calculate the two bivariates means.
##' @return 'ppStats' returns a data frame with numeric vectors. The first 'r' represents the neighborhood distance definition; '' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' ppStatsBi(x = runif(100), y = runif(100), code = factor(rep(c("type1", "type2"), each = 50)), xlim = c(0,1), ylim = c(0,1), rMax = 0.25, step = 0.02)
##' }
##' @export
ppStatsBi <- function(x, y, code,  xlim = c(0,1), ylim = c(0,1), rMax = min(c(xlim[2], ylim[2]))/4, step = rMax/100, ways = c("order", "reverse", "both"))
{
    ways <- match.arg(ways)
    if(length(x) != length(y))
    {
        stop("x and y should be numeric and have the same length")
    }
    if(any(x > xlim[2] | x < xlim[1] | y > ylim[2] | y < ylim[1]))
    {
        stop("Some points are out of border define in 'xlim' and 'ylim'")
    }
    mN <- meanNeighborsBi(x = x, y = y, code = code, xlim= xlim, ylim= ylim, rMax = rMax, step = step, ways = ways)
    spp <- unique(code)
    if(ways == "reverse")
    {
        spp <- rev(spp)
    }
    ## sp1_2 <- assign(paste(spp[1], spp[2], sep = " x "))
    ## sp2_1 <- assign(paste(spp[2], spp[1], sep = " x "))
    nsp1 <- sum(code == spp[1])
    nsp2 <- sum(code == spp[2])
    mN$K_Obs_12 <- mN$meanNeighbors_12 * (diff(xlim)* diff(ylim))/nsp2
    mN$L_Obs_12 <- sqrt(mN$K_Obs_12/pi) - mN$r
    mN$O_Obs_12 <- mN$L_Obs_12 - c(0, mN$L_Obs_12[- (length(mN$L_Obs_12))])
    if(ways == "both")
    {
        mN$K_Obs_21 <- mN$meanNeighbors_21 * (diff(xlim)* diff(ylim))/nsp1
        mN$L_Obs_21 <- sqrt(mN$K_Obs_21/pi) - mN$r
        mN$O_Obs_21 <- mN$L_Obs_21 - c(0, mN$L_Obs_21[- (length(mN$L_Obs_21))])
    }
    return(mN)
}
# Minimum Contrast function
##' @rdname pointPattern
##' @export
minContrast <- function(xyStat, par)
{
    theoT <- pi * xyStat$r^2 + (1 - exp(-1 * xyStat$r^2/(4 * par[2L])))/par[1L]
    #pi*(data$r^2) +  ((1 - exp(-data$r^2/4*par[2L]^2))/par[1L])
    mean((xyStat$K_Obs^0.25 - theoT^0.25)^2)
}
##########################################################
# Estimates Thomas Process parameters by Minimum Contrast Method
# kappa: intensity (lambda) poisson parameter (per unit of area), not the mean number of clusters!
# ncluster: mean number of cluster estimated by kappa
# sigma: standard deviation of random displacement (x and y) of point from its cluster center
# nCluster: mean per unit of area (cels)
##' @rdname pointPattern
##' @export
estimaThomas <- function(x, y, xlim, ylim, parIni = NULL)
{
    xyStat <- ppStats(x = x, y = y, xlim= xlim, ylim = ylim)
    rMax <- max(xyStat$r)
    np <- length(x)
    if(is.null(parIni))
    {
    parIni <- c(1, max(xyStat$r)^2/20)    
    }
    parOptim <- optim(par = parIni, fn = minContrast, xyStat = xyStat)$par#, method="L-BFGS-B", lower = c(2,0), upper = c(np, (rMax)^2*2))$par
    nCels <- diff(xlim) * diff(ylim)
    nClusters <- parOptim[1L] * nCels
    mu <- length(x)/nClusters
    return(c(kappa = parOptim[1L], sigma = sqrt(parOptim[2L]), nClusters = nClusters, mu = mu))
}
###################
## Simula Envelope
###################
##' @title
##' Confidence enevelope for point pattern statistics
##'
##' @description 
##'Compute 95\% confidence envelope for point pattern statistics (K-Ripley, L-Ripley and O-Ring), cluster process (Thomas) and  for a bivariate point pattern. Null distribution is based on simulation of complete spatial randomness for K-Ripey L-Ripley and O-Ring. Thomas process is based on random simulation of points using estimate parameters: $\kappa$ the intensity of clusters and $\sigma$ standard deviation position of point from the center of the cluster. Bivariate points patterns null model are simulated keeping pattern 1 points fixed and positioning points type 2 at random 
##'
##' @param x,y Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.
##' @param code Factor vector with two levels and same length as 'x' and 'y'
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax Maximum distance for neighborhood definition.
##' @param step Increase distance interval for neighborhood.
##' @param ways Type of bivariate calculation: 'order' uses first levels as target points; 'reverse' uses second level as target points; 'both' calculate the two bivariates means.
##' @param type Statistic to be evaluated. Character string of one of  those options:  'K-Ripley', 'L-Ripley',  'O-Ring' or 'Thomas'.
##' @return 'ppStats' returns a data frame with numeric vectors. The first 'r' represents the neighborhood distance definition; '' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
##' @name envelopeSim
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' envelopeSim(x = runif(100), y = runif(100), code = factor(rep(c("type1", "type2"), each = 50)), xlim = c(0,1), ylim = c(0,1), rMax = 0.25, step = 0.02, type = "K-Ripley")
##' }
##' @export
envelopeSim <- function(x, y, xlim = c(0,1), ylim = c(0,1), rMax = NULL, step = NULL, type = c("K-Ripley", "L-Ripley", "O-Ring", "Thomas"), anima = TRUE, nsim = 200, interval = 0.95)
{
    if(!is.numeric(interval)| interval >= 1 | interval <= 0)
    {
       stop("'interval' argument must be numeric between 0 and 1") 
    }
    qMin <- (1- interval)/2
    qMax <- 1 - qMin
    statName <- match.arg(type)
    statShort <- paste(substr(statName, start = 1, stop = 1), "_Obs", sep = "")
    if(statShort == "T_Obs")
    {
        statShort <- "K_Obs"
    }
    statSim <- paste(substr(statName, start = 1, stop = 1), "sim_", sep = "")
    np <- length(x)
    pb <- txtProgressBar(title = "Simulations done", label= "starting simulations", min =  0, max = nsim, initial = 0, style = 3)
    if(is.null(rMax))
    {
        rMax <-  min(c(diff(xlim), diff(ylim)))/4
    }
    if(is.null(step))
    {
        step <- rMax/100
    }
    ## Estimating Point Pattern Statistics for data
    resObs <- ppStats(x= x, y = y, xlim = xlim, ylim = ylim, rMax = rMax, step = step)
    ## Estimating for  Thomas Process
    if(statName == "Thomas")
    {
        parThomas <- estimaThomas(x = x, y = y,xlim = xlim, ylim = ylim , parIni = NULL)
        theoT <- pi * resObs$r^2 + (1 - exp(-resObs$r^2/(4 * (parThomas[2L])^2)))/parThomas[1L]
    }
    if(anima)
    {
        ymax <- max(resObs[, statShort])
        if(statName == "K-Ripley" | statName == "Thomas")
        {
            ymin = 0
        }
        else
        {
            ymin = min(resObs[, statShort], -1 * ymax/2)
        }
        x11(width = 14)
        par(mar = c(3, 3, 2, 2), las = 1, mfrow = c(1,2))
        plot(x, y, xlim= xlim, ylim= ylim, pch=19, col=rgb(1,0,0,0.3), cex = 1.5, axes=FALSE, xlab="", ylab="")
        par1 <- par(c("mfg", "plt", "usr"))
        axis(1, at=seq(xlim[1], xlim[2],len=5), lwd=0)
        axis(2, at=seq(xlim[1], xlim[2],len=5), lwd=0, las = 1)
        grid(nx = 4, ny = 4, col = rgb(0,0,1))
        abline(v = c(xlim[1], xlim[2]), col = rgb(0,0,1), lty = 1)
        abline(h = c(ylim[1], ylim[2]), col = rgb(0,0,1), lty = 1)
        par(mar = c(4.5, 4, 2, 2), las = 1)
        plot(resObs$r, resObs[,statShort], pch=19, col=rgb(0,0,1,0.3), cex = 1, axes=FALSE, xlab="Raio (m)", ylab=statName, cex.lab = 1.2, ylim = c(ymin, ymax))
        par2<- par(c("mfg", "plt", "usr"))
        axis(1, at =seq(0, rMax, by = step*5))
        axis(2)
        if(statName == "K-Ripley" )
        {
            lines(x = resObs$r, y = (resObs$r)^2 * pi, lty = 2, col = "red", lwd = 2)
        }
        if(statName == "Thomas")
        {
            lines(x = resObs$r, theoT, lty = 2, col = "red", lwd = 2)
        }
        if(statName == "L-Ripley" | statName == "O-Ring")
        {
            abline(h = 0, lty = 2, col = "red", lwd = 2)
        }
    }
    for(i in 1:nsim)
    {
        if(statName == "Thomas")
        {
            mu <- parThomas["mu"]
            npc <- rpois(parThomas["nClusters"], mu)
            xp <- rep(runif(parThomas["nClusters"], xlim[1], xlim[2]), times= npc) 
            yp <- rep(runif(parThomas["nClusters"], ylim[1], ylim[2]), times= npc)
            x0 <- round(xp + rnorm(n= sum(npc), mean = 0, sd = parThomas["sigma"]), 2)
            y0 <- round(yp + rnorm(n= sum(npc), mean = 0, sd = parThomas["sigma"]), 2)
            duplVF <- (duplicated(paste(x0,y0)))
            x0[duplVF] <- x0[duplVF] + sample(c(diff(xlim)/100, -1 * diff(xlim)/100), size = sum(duplVF), replace = TRUE)
            y0[duplVF] <- y0[duplVF] + sample(c(diff(xlim)/100, -1 * diff(xlim)/100), size = sum(duplVF), replace = TRUE) 
            xRand <- x0[(x0 > xlim[1] & x0 <= xlim[2]) & (y0 > ylim[1] & y0 <= ylim[2])]
            yRand <- y0[(x0 > xlim[1] & x0 <= xlim[2]) & (y0 > ylim[1] & y0 <= ylim[2])]
        }
        else{
        xRand <- runif(n = np, min = xlim[1], max = xlim[2])
        yRand <- runif(n = np, min = ylim[1], max = ylim[2])
        duplXY <- duplicated(paste(xRand, yRand))
        while(any(duplXY))
        {
            xRand[duplXY] <- runif(n = sum(duplXY), min = xlim[1], max = xlim[2])
            yRand[duplXY] <- runif(n = sum(duplXY), min = xlim[1], max = xlim[2])
            duplXY <- duplicated(paste(xRand, yRand))
        }
        }
        resSim <- ppStats(x= xRand, y = yRand, xlim = xlim, ylim = ylim, rMax = rMax, step = step)[, statShort]
        if(anima)
        {
            par(par1)
            rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], col = rgb(1, 1, 1), lwd = 0)
            points(x, y, pch=19, col=rgb(1,0,0,0.3), cex = 1.5)
            points(xRand, yRand, pch = 16, col = rgb(0,0,0,.5))
            abline(v = c(xlim[1], xlim[2]), col = rgb(0,0,1), lty = 1)
            abline(h = c(ylim[1], ylim[2]), col = rgb(0,0,1), lty = 1)
            mtext(text = "|", side = 3, at = (i/nsim)*xlim[2] ,line = 0, cex = 1.5)
            par(par2)
            lines(x = resObs$r, resSim, col = rgb(0,0,0,.1))
        }
        resObs[,paste(statSim, i, sep = "")] <- resSim
        lbsim <- paste("simulation:", i,  "from", nsim)
        setTxtProgressBar(pb, i, label = lbsim)
    }
    env <- apply(resObs[,grep("sim_", names(resObs))], 1, quantile, probs=c(qMin, qMax))
    if(anima)
    {
      plot(x, y, xlim= xlim, ylim= ylim, pch=19, col=rgb(1,0,0,0.3), cex = 1.5, axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(xlim[1], xlim[2],len=5), lwd=0)
      axis(2, at=seq(xlim[1], xlim[2],len=5), lwd=0, las = 1)
      grid(nx = 4, ny = 4, col = rgb(0,0,1))
      abline(v = c(xlim[1], xlim[2]), col = rgb(0,0,1), lty = 1)
      abline(h = c(ylim[1], ylim[2]), col = rgb(0,0,1), lty = 1)
      par(mar = c(4.5, 4, 2, 2), las = 1)
      plot(resObs$r, resObs[,statShort], pch=19, col=rgb(0,0,1,0.7), cex = 1, xlab="Raio (m)", ylab = ifelse(statName == "Thomas", "K-Ripley", statName), cex.lab = 1.2, ylim = c(ymin, ymax), bty = "l")
      polygon(x = c(resObs$r, rev(resObs$r)), y = c(env["2.5%",], rev(env["97.5%",])), col = rgb(0,0,0, .15), border = rgb(0,0,0, .25) )
      points(resObs$r, resObs[,statShort], pch=19, col=rgb(0,0,1,0.7))
      if(statName == "K-Ripley" )
      {
          lines(x = resObs$r, resObs$r^2 * pi, lty = 2, col = "red", lwd = 1.5)
          mtext("Complete Spatial Randomness", side = 3,line = 0, cex = 1.5)
      }
      if(statName == "Thomas")
      {
          lines(x = resObs$r, theoT, lty = 2, col = "red", lwd = 1.5)
          mtext("Thomas Cluster Process", side = 3,line = 0, cex = 1.5)
      }
      if(statName == "L-Ripley" | statName == "O-Ring")
      {
          abline(h = 0, lty = 2, col = "red", lwd = 1.5)
          mtext("Complete Spatial Randomness", side = 3,line = 0, cex = 1.5)
      }
      xyleg <- ifelse(statName == "O-Ring",  "topright", "topleft")
      legend(xyleg, legend = c("Observed", "Expected", "95% Confidence Envelope"), pch = c(19, -1, 15), lty = c(0, 2, 0), col = c(rgb(0,0,1), rgb(1,0,0), rgb(0,0,0,.25)), bty = "n", pt.cex = c(1,1, 2), xjust = 0.5) 
    }
    close(pb)
    return(resObs)
}
######################
## Envelope Bivariate
## Simulated  K_ij(r): 'i' is fixed and 'j' is CRD
######################
##' @rdname envelopeSim
##' @export
envelopeBi <- function(x, y, code, codeOrder = NULL ,xlim = c(0,1), ylim = c(0,1), rMax = NULL, step = NULL, type = c("K-Ripley", "L-Ripley", "O-Ring"), anima = TRUE, nsim = 200, interval = .95)
{
    if(!is.numeric(interval)| interval >=1 | interval <= 0)
    {
       stop("'interval' argument must be numeric between 0 and 1") 
    }
    if(is.null(codeOrder))
    {
        codeOrder <- unique(as.factor(code))
        
    }
    qMin <- (1- interval)/2
    qMax <- 1 - qMin
    #   spcode <- unique(as.factor(code))
    np <- table(code)
    typeVF <- code == codeOrder[1]
    np2 <- np[codeOrder[2]]
    statName <- match.arg(type)
    statShort <- paste(substr(statName, start = 1, stop = 1), "_Obs_12", sep = "")
    ## if(statShort == "T_Obs")
    ## {
    ##     statShort <- "K_Obs"
    ## }
    statSim <- paste(substr(statName, start = 1, stop = 1), "sim_", sep = "")
#   np <- length(x)
    pb <- txtProgressBar(title = "Simulations done", label= "starting simulations", min =  0, max = nsim, initial = 0, style = 3)
    if(is.null(rMax))
    {
        rMax <-  min(c(diff(xlim), diff(ylim)))/4
    }
    if(is.null(step))
    {
        step <- rMax/100
    }
    ## Estimating Point Pattern Statistics for data
    resObs <- ppStatsBi(x= x, y = y, code  = code, xlim = xlim, ylim = ylim, rMax = rMax, step = step, ways = "order")
    if(anima)
    {
        ymax <- max(resObs[, statShort])
        if(statName == "K-Ripley")
        {
            ymin = 0
        }
        else
        {
            ymin = min(resObs[, statShort], -1 * ymax/2)
        }
        x11(width = 14)
        par(mar = c(3, 3, 2, 2), las = 1, mfrow = c(1,2))
        plot(x[typeVF], y[typeVF], xlim= xlim, ylim= ylim, pch=19, col=rgb(1,0,0,0.3), cex = 1.5, axes=FALSE, xlab="", ylab="")
        points(x[!typeVF], y[!typeVF], pch=16, col=rgb(0,0,1,0.3), cex = 1)
        par1 <- par(c("mfg", "plt", "usr"))
        axis(1, at=seq(xlim[1], xlim[2],len=5), lwd=0)
        axis(2, at=seq(xlim[1], xlim[2],len=5), lwd=0, las = 1)
        grid(nx = 4, ny = 4, col = rgb(0,0,1))
        abline(v = c(xlim[1], xlim[2]), col = rgb(0,0,1), lty = 1)
        abline(h = c(ylim[1], ylim[2]), col = rgb(0,0,1), lty = 1)
        par(mar = c(4.5, 4, 2, 2), las = 1)
        plot(resObs$r, resObs[,statShort], pch=19, col=rgb(0,0,1,0.3), cex = 1, axes=FALSE, xlab="Raio (m)", ylab=statName, cex.lab = 1.2, ylim = c(ymin, ymax))
        par2<- par(c("mfg", "plt", "usr"))
        axis(1, at =seq(0, rMax, by = step*5))
        axis(2)
        if(statName == "K-Ripley")
        {
            lines(x = resObs$r, y = (resObs$r)^2 * pi, lty = 2, col = "red", lwd = 2)
        }
        if(statName == "L-Ripley" | statName == "O-Ring")
        {
            abline(h = 0, lty = 2, col = "red", lwd = 2)
        }
    }
    for(i in 1:nsim)
    {
        xRand <- x
        yRand <- y
        xRand[!typeVF] <- runif(n = np2, min = xlim[1], max = xlim[2])
        yRand[!typeVF] <- runif(n = np2, min = ylim[1], max = ylim[2])
        duplXY <- duplicated(paste(xRand, yRand))
        while(any(duplXY))
        {
            xRand[duplXY] <- runif(n = sum(duplXY), min = xlim[1], max = xlim[2])
            yRand[duplXY] <- runif(n = sum(duplXY), min = xlim[1], max = xlim[2])
            duplXY <- duplicated(paste(xRand, yRand))
        }
        resSim <- ppStatsBi(x= xRand, y = yRand, code = code, xlim = xlim, ylim = ylim, rMax = rMax, step = step, ways = "order")[, statShort]
        if(anima)
        {
            par(par1)
            rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], col = rgb(1, 1, 1), lwd = 0)
            points(x[typeVF], y[typeVF], pch=19, col=rgb(1,0,0,0.3), cex = 1.5)
            points(x[!typeVF], y[!typeVF], pch=16, col=rgb(0,0,1,0.3), cex = 1)
 
            points(xRand[!typeVF], yRand[!typeVF], pch = 16, col = rgb(0,0,0,.3))
            abline(v = c(xlim[1], xlim[2]), col = rgb(0,0,1), lty = 1)
            abline(h = c(ylim[1], ylim[2]), col = rgb(0,0,1), lty = 1)
            mtext(text = "|", side = 3, at = (i/nsim)*xlim[2] ,line = 0, cex = 1.5)
            par(par2)
            lines(x = resObs$r, resSim, col = rgb(0,0,0,.1))
        }
        resObs[,paste(statSim, i, sep = "")] <- resSim
        lbsim <- paste("simulation:", i,  "from", nsim)
        setTxtProgressBar(pb, i, label = lbsim)
    }
    env <- apply(resObs[,grep("sim_", names(resObs))], 1, quantile, probs=c(qMin, qMax))
    if(anima)
    {
      plot(x[typeVF], y[typeVF], xlim= xlim, ylim= ylim, pch=19, col=rgb(1,0,0,0.3), cex = 1.5, axes=FALSE, xlab="", ylab="")
      points(x[!typeVF], y[!typeVF], pch=16, col=rgb(0,0,1,0.3), cex = 1)
      axis(1, at=seq(xlim[1], xlim[2],len=5), lwd=0)
      axis(2, at=seq(xlim[1], xlim[2],len=5), lwd=0, las = 1)
      grid(nx = 4, ny = 4, col = rgb(0,0,1))
      abline(v = c(xlim[1], xlim[2]), col = rgb(0,0,1), lty = 1)
      abline(h = c(ylim[1], ylim[2]), col = rgb(0,0,1), lty = 1)
      par(mar = c(4.5, 4, 2, 2), las = 1)
      plot(resObs$r, resObs[,statShort], pch=19, col=rgb(0,0,1,0.7), cex = 1, xlab="Raio (m)", ylab = ifelse(statName == "Thomas", "K-Ripley", statName), cex.lab = 1.2, ylim = c(ymin, ymax), bty = "l")
      polygon(x = c(resObs$r, rev(resObs$r)), y = c(env["2.5%",], rev(env["97.5%",])), col = rgb(0,0,0, .15), border = rgb(0,0,0, .25) )
      points(resObs$r, resObs[,statShort], pch=19, col=rgb(0,0,1,0.7))
      if(statName == "K-Ripley" )
      {
          lines(x = resObs$r, resObs$r^2 * pi, lty = 2, col = "red", lwd = 1.5)
          mtext("Complete Spatial Randomness", side = 3,line = 0, cex = 1.5)
      }
      if(statName == "L-Ripley" | statName == "O-Ring")
      {
          abline(h = 0, lty = 2, col = "red", lwd = 1.5)
          mtext("Complete Spatial Randomness", side = 3,line = 0, cex = 1.5)
      }
      xyleg <- ifelse(statName == "O-Ring",  "topright", "topleft")
      legend(xyleg, legend = c("Observed", "Expected", "95% Confidence Envelope"), pch = c(19, -1, 15), lty = c(0, 2, 0), col = c(rgb(0,0,1), rgb(1,0,0), rgb(0,0,0,.25)), bty = "n", pt.cex = c(1,1, 2), xjust = 0.5) #x = resObs$r[2], y = resObs[nrow(resObs) - 2, statShort]
    }
    close(pb)
    return(resObs)
}
################################
## Point Pattern Data Simulation
################################
##' @title
##' Simulate Point Pattern Data
##'
##' @description 
##'Simulate catesian coordinate data 'x' and 'y' for randon (Poisson) point pattern and cluster process (Thomas) for uni or bivariate point pattern. Thomas process is based on random simulation of points using estimate parameters: number and size of clusters. Number of clusters could be given in two ways, using 'nCluster' parameters or, if this is 'NULL' provide by the number of code one points (parents). 
##'
##' @param npts Integer vectors with one (univariate) or two values (bivariate). If there is zero in one of two values only the posite integer is considered.  
##' @param code Factor vector with two levels and same length as 'x' and 'y'
##' @param xlim,ylim Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.
##' @param rMax Maximum distance for neighborhood definition.
##' @param step Increase distance interval for neighborhood.
##' @param ways Type of bivariate calculation: 'order' uses first levels as target points; 'reverse' uses second level as target points; 'both' calculate the two bivariates means.
##' @param type Statistic to be evaluated. Character string of one of  those options:  'K-Ripley', 'L-Ripley',  'O-Ring' or 'Thomas'.
##' @return 'ppStats' returns a data frame with numeric vectors. The first 'r' represents the neighborhood distance definition; '' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
##' @name envelopeSim
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @references Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
##' Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
##' @keywords neighborhood analysis
##' @examples
##' \dontrun{
##' envelopeSim(x = runif(100), y = runif(100), code = factor(rep(c("type1", "type2"), each = 50)), xlim = c(0,1), ylim = c(0,1), rMax = 0.25, step = 0.02, type = "K-Ripley")
##' }
##' @export

ppSim <- function(npts = c(100, 0), xlim = c(0,1), ylim = c(0,1), simType = c("Random", "Cluster"), clusterSize = NULL, nClusters = NULL)
{
    sT <- match.arg(simType)
    npts <- npts[npts > 0]
    ncode <- length(npts)
    if(ncode == 0)
    {
    stop("Argument 'npts' should have at least one integer more than zero")    
    }
    if(ncode == 2)
    {
        cat("generating a bivariate ", simType, " point pattern with ", npts[1], " code 1 points and", npts[2], " code 2 points\n")
    }
    if(ncode == 1)
    {
        cat("generating a univariate", simType, " point pattern with ", npts[1], " points\n")
    }
    if(ncode == 0 | ncode > 2)        
    {
        stop("The object 'npts' need to have one or two integers, only uni or bivariate point pattern simulation is available at the moment")
    }
    if(sT == "Random")
    {
        xRand <- runif(n = sum(npts), min = xlim[1], max = xlim[2])
        yRand <- runif(n = sum(npts), min = ylim[1], max = ylim[2])
        duplXY <- duplicated(paste(xRand, yRand))
        #code <- ifelse(ncode == 1, rep(c(1,2), npts), rep(1, npts))
        while(any(duplXY))
        {
            xRand[duplXY] <- runif(n = sum(duplXY), min = xlim[1], max = xlim[2])
            yRand[duplXY] <- runif(n = sum(duplXY), min = xlim[1], max = xlim[2])
            duplXY <- duplicated(paste(xRand, yRand))
        }
    }
    if(sT == "Cluster")
    {
        if(is.null(clusterSize) | clusterSize > max(c(xlim, ylim)))
        {
            stop("The cluster size must be a number less than region dimensions")
        }
        if(ncode == 1 & is.null(nClusters))
        {
            stop("The number of clusters is a necessary parameter and must be provide either by 'nClusters' argument or by the number in the first position of 'npts' vector argument")
        }
        if(is.null(nClusters))
        {
        nClusters <- npts[1]    
        }
        mu <- npts[ncode]/nClusters
        npc <- rpois(nClusters, mu)
        xCl <- runif(nClusters, xlim[1], xlim[2])
        yCl <- runif(nClusters, ylim[1], ylim[2])
        xp <- rep(xCl, times = npc) 
        yp <- rep(yCl, times = npc)
        x0 <- round(xp + rnorm(n= sum(npc), mean = 0, sd = clusterSize/2), 2)
        y0 <- round(yp + rnorm(n= sum(npc), mean = 0, sd = clusterSize/2), 2)
        duplVF <- (duplicated(paste(x0,y0)))
        while(any(duplVF))
        {
            x0[duplVF] <- x0[duplVF] + sample(c(0.01, -0.01),  size = sum(duplVF), replace = TRUE)
            y0[duplVF] <- y0[duplVF] + sample(c(0.01, -0.01), size = sum(duplVF), replace = TRUE) 
            duplVF <- (duplicated(paste(x0,y0)))
        }
        xRand <- x0[(x0 > xlim[1] & x0 <= xlim[2]) & (y0 > ylim[1] & y0 <= ylim[2])]
        yRand <- y0[(x0 > xlim[1] & x0 <= xlim[2]) & (y0 > ylim[1] & y0 <= ylim[2])]
        if(ncode == 2)
        {
            xRand <- c(xCl, xRand)
            yRand <- c(yCl, yRand)
        }
    }
    code <- rep(1, length(xRand))
    if(ncode == 2)
    {
        code[(npts[1] + 1):length(xRand)] <- 2
    }
    data.frame(x = xRand, y = yRand, code = code)
}

#xy <- ppSim(npts = c(200), xlim = c(0,100), ylim =c(0,100), simType = "Cluster", clusterSize = 10)#, nClusters = 20)
#plot(x = xy$x, y = xy$y, col = xy$code, pch  = 16)
###################
## end of code ####
###################
