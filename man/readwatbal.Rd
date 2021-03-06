% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readwatbal.R
\name{readwatbal}
\alias{readwatbal}
\title{Reads the watbal.dat MAESPA output file}
\usage{
readwatbal(filename = "watbal.dat")
}
\arguments{
\item{filename}{Default name of the (half-)hourly water balance output file.}
}
\value{
Returns a dataframe.
}
\description{
Reads the hourly water balance output file ("watbal.dat").
}
\examples{


\dontrun{

# Simple as this:
mywatbalresult <- readwatbal()

# If you want to select the water balance file with a menu:
readwatbal(file.choose())

}
}
\author{
Remko Duursma
}
\keyword{utilities}
