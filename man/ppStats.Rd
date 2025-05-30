% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighborsPointPattern.R
\name{ppStats}
\alias{ppStats}
\title{Point Pattern Statistics Calculation}
\usage{
ppStats(
  x,
  y,
  xlim = c(0, 1),
  ylim = c(0, 1),
  rMax = min(c(xlim[2], ylim[2]))/4,
  step = rMax/100
)
}
\arguments{
\item{x, y}{Numeric vectors with 'x' and 'y' coordinates in a cartesian space. They must have the same length.}

\item{xlim, ylim}{Numeric vectors with two values defining the 'x' and 'y' axis limit in the cartesian plane.}

\item{rMax}{maximum distance for neighborhood definition.}

\item{step}{increase distance size for neighborhood.}

\item{ways}{type of bivariate calculation: 'order' uses first levels as target points; 'reverse' uses second level as target points; 'both' calculate the two bivariates means.}
}
\value{
'ppStats' returns a data frame with numeric vectors. The first 'r' represents the neighborhood distance definition; '' the mean number of neighbors for each 'r' distance. This count uses the torus border correction.
}
\description{
Compute point pattern multiscale statistics K-Ripley, L-Ripley and O-Ring. K-Ring is a statistics based on mean number of neighbors around each point in a function of the neighbourhood distances divide by the intensity of points $\lambda$. The expected K-Ripley for complete random distribution is equal $pi * r^2$. L-Ripley is define here as $\sqrt{K_{(r)}/\pi} - r$ an parametrization to have expectance equal zero. O-Ring calculate L-Ripley for a ring of width equal to `step` argument.
}
\examples{
\dontrun{
ppStats(x = runif(100), y = runif(100), xlim = c(0,1), ylim = c(0,1), rMax = 0.25, step = 0.02)
}
}
\references{
Baddeley, A.; Rubak, E; Turner, R. 2016. Spatial Point Patterns: Methodology and Applications with R. CRC Press.
Wiegand, T. & Moloney, K.A. 2014. Handbook of Spatial Point-Pattern Analysis in Ecology. CRC Press.
}
\author{
Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
}
\keyword{analysis}
\keyword{neighborhood}
