\name{readwatbal}
\alias{readwatbal}
\title{ Reads the watbal.dat MAESPA output file}
\description{
  Reads the hourly water balance output file ("watbal.dat").
}
\usage{
readwatbal(filename = "watbal.dat")
}
\arguments{
  \item{filename}{ Default name of the (half-)hourly water balance output file.}
}

\value{
  Returns a dataframe.
}
\author{Remko Duursma}

\examples{

\dontrun{

# Simple as this:
mywatbalresult <- readwatbal()

# If you want to select the water balance file with a menu:
readwatbal(file.choose())

}
}

\keyword{ utilities }