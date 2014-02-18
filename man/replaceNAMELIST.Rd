\name{replaceNameList}
\alias{replaceNameList}
\title{Replaces a namelist}
\usage{
replaceNameList(namelist = NA, datfile = NA, vals = list())
}
\arguments{
  \item{namelist}{Name of the namelist.}

  \item{datfile}{Name of the input file.}

  \item{vals}{A list of values (see example below).}
}
\value{
Nothing is returned. The input file is modified.
}
\description{
Replaces the whole namelist in an input file. All
parameters in the namelist must be provided, otherwise
MAESTRA/MAESPA will likely crash.
}
\examples{
\dontrun{
replaceNameList(namelist="aerodyn",
    datfile="trees.dat", vals=list(zht=30,zpd=3,z0ht=0.6))
}
}
\author{
Remko Duursma
}
\references{
See Belinda Medlyn's MAESTRA homepage at:
\url{http://www.bio.mq.edu.au/maestra/ }
}
\seealso{
\code{\link{replacePAR}}
}
\keyword{utilities}

