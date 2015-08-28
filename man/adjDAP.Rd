\name{adjDAP}
\alias{adjDAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adjDAP(dados, dap = "dbh", censos = c("01", "02"), sufixo = ".mm")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dados}{
%%     ~~Describe \code{dados} here~~
}
  \item{dap}{
%%     ~~Describe \code{dap} here~~
}
  \item{censos}{
%%     ~~Describe \code{censos} here~~
}
  \item{sufixo}{
%%     ~~Describe \code{sufixo} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dados, dap = "dbh", censos = c("01", "02"), sufixo = ".mm") 
{
    vrep <- Vectorize(rep.int, "times")
    namedbh = paste(dap, censos, sufixo, sep = "")
    namedados <- names(dados)
    dbhano <- names(dados)[grep("dbh", names(dados))]
    nc <- substr(dbhano[!dbhano \%in\% namedbh], start = 4, stop = 5)
    for (k in nc) {
        npos <- grep(k, names(dados))
        dados <- dados[, -npos]
    }
    dbh = dados[, namedbh]
    dbh[dbh == "" | dbh == " " | is.null(dbh)] <- NA
    ncensos = length(censos)
    ntree = dim(dados)[1]
    nfuste <- matrix(NA, ncol = ncensos, nrow = ntree)
    if (ncensos > 1) {
        ldbh0 <- apply(dbh, 2, strsplit, split = ";")
        for (i in 1:ncensos) {
            nfuste[, i] <- sapply(ldbh0[[i]], length)
        }
        maxfuste <- apply(nfuste, 1, max)
        diffuste <- maxfuste - nfuste
        difvf <- diffuste > 0
        tdif <- apply(difvf > 0, 1, sum)
    }
    else {
        ldbh0 <- strsplit(dbh, split = ";")
        nfuste[, 1] <- sapply(ldbh0, length)
        maxfuste <- apply(nfuste, 1, max)
        tdif <- rep(0, ntree)
    }
    datanew = NULL
    for (j in 1:ntree) {
        dtree <- dados[j, ]
        dtree$fuste <- 1
        nf = maxfuste[j]
        if (nf > 1) {
            if (tdif[j] > 0) {
                dfuste <- diffuste[j, ]
                indf <- dfuste > 0
                fusteNA <- vrep(times = dfuste, ";NA")
                fusteNA <- sapply(fusteNA, paste, collapse = "")
                dbh[j, ] <- paste(dbh[j, ], fusteNA, sep = "")
            }
            ndtree <- paste(rep("dtree", nf), collapse = ", ")
            strtree <- paste("dtree <- rbind(", ndtree, ")", 
                sep = "")
            eval(parse(text = strtree))
            dtree$fuste = 1:nrow(dtree)
        }
        dtree[, namedbh] <- NA
        if (ncensos > 1) {
            newdbh <- unlist(sapply(dbh[j, ], strsplit, split = ";"))
        }
        else {
            newdbh <- unlist(sapply(dbh[j], strsplit, split = ";"))
        }
        newdbh[newdbh == "NA"] <- NA
        dtree[, namedbh] <- as.numeric(newdbh)
        datanew = rbind(datanew, dtree)
        cat("Aguarde! Processando arvore ", j, "  de um total de ", 
            ntree, "\n")
    }
    return(datanew)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
