\name{utilities}
\alias{delempty}
\alias{maybeQuote}
\alias{mayberead}
\alias{printme}
\alias{trim}

\title{Utility functions}
\description{
Several simple functions for handling strings etc.
}

\usage{
delempty(x)
maybeQuote(x)
mayberead(...)
printme(x, SEP=" ")
trim(str)
}
\arguments{
  \item{x}{A string, or a character vector}
  \item{str}{Also a string}
  \item{...}{A file name}
  \item{SEP}{Separator in printing the namelist}
}


\details{
\code{delempty} deletes elements of a vector that are empty strings.
\code{maybeQuote} returns the quoted string unless the argument can be successfully converted to numeric.
\code{printme} is a utility function for printing namelists.
\code{trim} deletes leading and trailing whitespace in a character string.
}
\author{ Remko Duursma }

\seealso{ \code{\link{readPAR}} }




\keyword{ utilities }