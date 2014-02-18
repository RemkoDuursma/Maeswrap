\name{readhrflux}
\alias{readhrflux}
\title{Reads the hrflux.dat MAESTRA/MAESPA output file}
\usage{
readhrflux(filename = "hrflux.dat")
}
\arguments{
  \item{filename}{Default name of the (half-)hourly output
  file.}
}
\value{
Returns a dataframe.
}
\description{
Reads the hourly output file (hrflux.dat).
}
\examples{
\dontrun{

# Simple as this:
mysim2 <- readhrflux()
}
}
\author{
Remko Duursma
}
\keyword{utilities}

