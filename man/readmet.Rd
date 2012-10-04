\name{readmet}
\alias{readmet}
\title{ Reads the met.dat input file}
\description{
  Reads the meteorological input data in the met.dat file.
}
\usage{
readmet(filename = "met.dat", nlines=-1)
}
\arguments{
  \item{filename}{ Default name of the met.dat file.}
  \item{nlines}{Optional, how many lines of the metfile to read?}
}
\value{
  Returns a dataframe.
}
\author{ Remko Duursma}

\examples{

\dontrun{

# Simple as pi:
metdata <- readmet()

}

}

\keyword{ utilities}
