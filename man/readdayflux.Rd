\name{readdayflux}
\alias{readdayflux}

\title{ Reads the dayflx.dat output file}
\description{
  Reads the dayflx.dat MAESTRA/MAESPA output file, returns a clean dataframe.
  Names of the variables are read from the Columns: line.
}
\usage{
readdayflux(filename = "dayflx.dat")
}
\arguments{
  \item{filename}{ Default name of the daily flux file.}
}

\value{
  Returns a dataframe.
}

\author{ Remko Duursma }

\examples{

\dontrun{
# Read it:
mysim1 <- readdayflux()
}

}

\keyword{ utilities }
