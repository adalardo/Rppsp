subquad <- function(dx, dy, size.split = 5, max.size = 20)
    {
        pos <- seq(5,max.size - size.split, by= size.split)
        #codx <- cody  <-rep(0, length(dx))
        ## for(i in pos){
        ##     codx <- codx + (dx > i)
        ##     cody <- cody + (dy > i)
        ## }
        dx.ind <- apply(sapply(pos, function(x){ifelse(dx >= x, 5,0)}), 1, sum)
        dy.ind <- apply(sapply(pos, function(x){ifelse(dy >= x, 5,0)}), 1, sum)
        dxy <- paste(dx.ind, "x",dy.ind, sep="")
        dxy[grep("NA", dxy)] <- NA
        return(dxy) 
    }
#dx = runif(10, 0, 20)
#dy = runif(10, 0, 20)
