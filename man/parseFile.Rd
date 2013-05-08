\name{parseFile}
\alias{parseFile}
\title{Parse an input file}
\usage{
  parseFile(fn)
}
\arguments{
  \item{fn}{Filename}
}
\value{
  Returns a named list, each element contains a namelist
  and its parameters.
}
\description{
  Takes an input file for MAESTRA/MAESPA, and reads all
  namelists into a nested list.
}
\author{
Remko Duursma, based on a suggestion by Andreas Ibrom.
}
\examples{
\dontrun{
# Parse a file
con <- parseFile("confile.dat")

# Namelists in the file
names(con)
}
}
\seealso{
  To read one namelist from a file, see
  \code{\link{readNameList}}.
}

