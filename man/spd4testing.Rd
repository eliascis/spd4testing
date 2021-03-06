% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spd4testing.R
\name{spd4testing}
\alias{spd4testing}
\title{Small Panel Data Set for Testing}
\usage{
spd4testing(N = 4, G = 2, Y = 5, missingY = FALSE, missingX = FALSE)
}
\arguments{
\item{N}{Number of individuals, id}

\item{G}{Number of groups, gid}

\item{Y}{Number of years, year}

\item{missingY}{create missing outcome values for one observation in dataset}

\item{missingX}{create missing explanatory values for one observation in dataset}
}
\value{
data.frame
}
\description{
spd4testing creates a panel data set with individual observations over time that belong to different groups.
Individuals are coded with the variable "id". The time variable is given by "year". The group identifier is "gid".
The data consits of one dependent variable "y", and two explanatory variables "x" and "u".
In addtion spd4testing adds a constant factor "z" equal to one and weights "w" with values 1 or 0.5.
To simulate a more complex data sttructure it is possible to randomly create missing values in "y" and/ or "x".
}
\details{
N*Y observations. \cr
G is the number of groups. \cr
Each group has the same size: N/G. \cr
N/G must be a whole number.
}
\examples{
spd4testing()

spd4testing(missingY=TRUE,missingX=TRUE)

spd4testing(N=6,G=3,Y=4)

\dontrun{
  spd4testing(N=6,G=4,Y=4) }

library(plm)
d <- spd4testing(missingY=FALSE,missingX=TRUE)
e<-plm(y ~ x + factor(year)*factor(gid), data=d, model="fd")
summary(e)


citation("spd4testing")
}
\author{
Elías Cisneros <ec@elias-cisneros.de>
}
