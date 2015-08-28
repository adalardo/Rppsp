\name{fusteUnico}
\alias{fusteUnico}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shor format dataframe, tree as lines
}
\description{
Calculate basal area form multiple stem trees and return an equivalent dbh}
\usage{
fusteUnico(dados.fuste, tag = "tag", dap = "dbh", censos = c("01", "02"), sufixo = ".mm")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dados.fuste}{
  Long format data as stem each line
}
  \item{tag}{
  Column name for tag number
}
  \item{dap}{
  dbh column name
}
  \item{censos}{
 how census are describe in dbh name
}
  \item{sufixo}{
  any other name pattern for dbh column
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
short format datframe, same number of columns.
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
%%\examples{
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dataframe}
