\name{replaceNameList}
\alias{replaceNameList}
\title{ Replaces a namelist.}
\description{
  Replaces the whole namelist in an input file. All parameters in the namelist must be provided, otherwise
  MAESTRA/MAESPA will likely crash.
}
\usage{
replaceNAMELIST(namelist = NA, 
                datfile = NA, 
                vals = list())
}
\arguments{
  \item{namelist}{Name of the namelist.}
  \item{datfile}{ Name of the input file.}
  \item{vals}{ A list of values (see example below).}
}
\value{
  Nothing is returned. The input file is modified.
}
\references{See Belinda Medlyn's MAESTRA homepage at: \url{http://www.bio.mq.edu.au/maestra/ }}

\author{ Remko Duursma}

\seealso{ \code{\link{replacePAR}} }
\examples{

\dontrun{
replaceNAMELIST(namelist="aerodyn", 
    datfile="trees.dat", vals=list(zht=30,zpd=3,z0ht=0.6))
}
}

\keyword{ utilities }
