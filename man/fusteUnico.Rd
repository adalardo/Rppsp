\name{fusteUnico}
\alias{fusteUnico}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fusteUnico(dados.fuste, tag = "tag", dap = "dbh", censos = c("01", "02"), sufixo = ".mm")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dados.fuste}{
%%     ~~Describe \code{dados.fuste} here~~
}
  \item{tag}{
%%     ~~Describe \code{tag} here~~
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
function (dados.fuste, tag = "tag", dap = "dbh", censos = c("01", 
    "02"), sufixo = ".mm") 
{
    namedbh = paste(dap, censos, sufixo, sep = "")
    abasal.cm <- aggregate(dados.fuste[, namedbh], list(tag = dados.fuste[, 
        tag]), FUN = function(x) {
        sum(pi * (x/20)^2, na.rm = TRUE)
    })
    abasal.cm[abasal.cm == 0] <- NA
    abasal.cm[, namedbh] <- round((4 * abasal.cm[, namedbh]/pi)^(1/2), 
        2)
    namedbh.new = paste(dap, censos, ".cm", sep = "")
    dados.cm <- dados.fuste[dados.fuste$fuste == 1, !(names(dados.fuste) \%in\% 
        c(namedbh, "fuste"))]
    dados.cm[, namedbh.new] <- abasal.cm[, -1]
    return(dados.cm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
