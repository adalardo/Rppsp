\name{adjDAP}
\alias{adjDAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stem as lines
}
\description{
Create a dataframe with each stem as lines for multiple stem tres - stem long format
}
\usage{
adjDAP(dados, dap = "dbh", censos = c("01", "02"), sufixo = ".mm")
}
\arguments{
  \item{dados}{
duckewiki database format dataframe
}
  \item{dap}{
how dbh is call in the dataframe format, for instance if dbh is in column dbh2012, dbh = "dbh"
}
  \item{censos}{
How census are describe in dataframe  
}
  \item{sufixo}{ 
  Any other pattern in dbh column name
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Return a dataframe longer or with the same size as original data; each new line is a stem from multiple stem tree   
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alexandre Adalardo de Oliveira <aleadalardo@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dados<- data.frame(sp= rep(c("sp1", "sp2", "sp3"), 4), dbh01.mm= rep(c("110;12;130;14", "140;13;12", "14;13", "14"), 3), dbh02.mm= rep(c("150;16;17;18", "NA", "18", "18"), 3) , alt=seq(10, 50, len=12), date01="10-10-2002", date02="10-10-2005", status01= "A", status02 =sample(c(rep("A", 10), "D","D")), stringsAsFactors = FALSE, tag=paste(0, 1:12, sep="") )
adjDAP(dados, dap = "dbh", censos = c("01", "02"), sufixo = ".mm")

\keyword{dataframe}
\keyword{data}% __ONLY ONE__ keyword per line
