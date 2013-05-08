\name{readPAR}
\alias{readPAR}
\alias{readNameList}
\title{ Reads the value of a parameter in a MAESTRA/MAESPA input file.}
\description{
  The \code{readPAR} function reads the value of any parameter in a namelist in one of the MAESTRA/MAESPA input files.
  Also works for other text files that have the FORTRAN namelist input structure.
  Optionally specifies in which namelist to look for the parameter.
  
  To read an entire namelist into a list, use the \code{readNameList} function.
  
}
\usage{
readPAR(datfile, parname, namelist = NA, fail=TRUE)
readNameList(datfile, namelist)
}
\arguments{
  \item{datfile}{ Name of the input file.}
  \item{parname}{ Name of the parameter. }
  \item{namelist}{ The namelist to look in, otherwise looks in the whole file. }
  \item{fail}{Logical. If TRUE, stops with an error when parameter is not found (if FALSE, returns NA)}.
}
\value{
	For \code{readPAR}, either one value, or a vector, depending on how many values are specified for the parameter in the
  input file.
  
  For \code{readNameList}, a named list.
}
\author{Remko Duursma. Thanks to Andreas Ibrom for reporting a bug.}

\seealso{ \code{\link{replacePAR}} }
\examples{

\dontrun{
# Read the number of trees in the plot:
readPAR("confile.dat", "notrees", "plot")

# Read the X and Y coordinates:
readPAR("confile.dat", "xycoords", "xy")

# Read entire namelist
readNameList("trees.dat", "plot")

}

}

\keyword{ utilities }
