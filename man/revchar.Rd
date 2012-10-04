\name{revchar}
\alias{revchar}

\title{ Reverse a character string.}
\description{
  Reverses the characters in a character string, unless a vector is supplied,
  in which case reverses the element of the vector.
}
\usage{
revchar(x)
}
\arguments{
  \item{x}{ A character vector (typically of length 1). }
}
\details{
  When a character vector of length > 1 is provided, reverses the elements of the vector, not the
  characters itself.
}
\value{
A vector.
}
\references{ None. }
\author{ Remko Duursma}

\seealso{ \code{\link{rev.default}},\code{\link{substr}},\code{\link{strsplit}}  }
\examples{

\dontrun{
# Take a substring, counting from the end:
substrfromend <- function(x,start,stop)revchar(substr(revchar(x),start,stop))
substrfromend('filename.txt', 1,3)

# Check if a word is a palindrome:
s <- 'saippuakivikauppias'
s == revchar(s)

# A semordnilap:
cat('I am so stressed, I need to eat', revchar('stressed'),'\n')
}
}
\keyword{ methods }