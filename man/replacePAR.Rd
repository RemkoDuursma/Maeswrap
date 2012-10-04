\name{replacePAR}
\alias{replacePAR}
\title{Replace a parameter value}
\description{
  Replaces a parameter value in a MAESTRA/MAESPA input file. Also works for a vector of input values.  
}
\usage{
replacePAR(datfile, parname, namelist = NA, newval, noquotes=FALSE)
}
\arguments{
  \item{datfile}{Name of the input file. }
  \item{parname}{Name of the parameter to replace the value of.}
  \item{namelist}{Optional. In which namelist to look for the parameter. }
  \item{newval}{New value of the parameter. Can be a single value or a vector, or a matrix (see Details).}
  \item{noquotes}{Logical. If FALSE, does print quotes around character values.}
}
\details{
If the new parameter value(s) is a vector (or a single value), the values will be placed on a single line in the parameter file.
If instead a matrix is provided, each row of the matrix is placed on a separate line. 

WARNING : The input file is modified. Make sure to backup your original input files!

}
\value{
Nothing is returned. The inputfile is modified.
}
\author{ Remko Duursma}
\seealso{ \code{\link{replaceNAMELIST}}, \code{\link{readPAR}}  }
\examples{

\dontrun{
# Replace a parameter with a single value:
replacePAR("trees.dat", "notrees", "plot", newval=100)

# Replace a number of values:
replacePAR("trees.dat", "xycoords", "xy", newval=c(1,1,2,2,3,3))

# Replace a number of values in a different way : this may be useful in 
# more complicated programs,
# rr when reading a string from a file (that should be interpreted as a vector).
replacePAR("trees.dat", "xycoords", "xy", newval="1 1 2 2 3 3", noquotes=TRUE)

# Replace a parameter so that the new values are placed on multiple rows.
# Useful for tree namelists with multiple dates and multiple trees 
# (where each row corresponds to a tree, and each column to a date.)
m <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, byrow=TRUE)
replacePAR("trees.dat", "indivlarea", "values", newval=m)
}
}

\keyword{ utilities }
