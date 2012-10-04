\name{readhrflux}
\alias{readhrflux}
\title{ Reads the hrflux.dat MAESTRA/MAESPA output file}
\description{
  Reads the hourly output file (hrflux.dat).
}
\usage{
readhrflux(filename = "hrflux.dat")
}
\arguments{
  \item{filename}{ Default name of the (half-)hourly output file.}
}

\value{
  Returns a dataframe.
}
\author{Remko Duursma}

\examples{

\dontrun{

# Simple as this:
mysim2 <- readhrflux()
}
}

\keyword{ utilities }