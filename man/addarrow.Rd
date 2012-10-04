\name{addarrow}
\alias{addarrow}
\title{Adds an arrow to a plot}
\description{
  Specify the length and rotation (bearing) of the arrow, and it will be added to
  the current device. Works for 3D and 2D plots. Different from \code{arrows}, where
  begin and endpoints of the arrow need to be specified.
}
\usage{
addarrow(x0,y0,len,bearing,headlen=0.2*len,
headangle=25,Nlabel=TRUE,addto=c('rgl','plot','lattice'),...)
}
\arguments{
  \item{x0,y0}{Begin point coordinates.}
  \item{len}{Length of arrow.}
  \item{bearing}{Direction. See Details.}
  \item{headlen}{Length of arrow head.}
  \item{headangle}{(Half)-Angle of arrow head.}
  \item{Nlabel}{Logical. Add a label 'N' or not.}
  \item{addto}{One of 'rgl', 'plot', or 'lattice', or first letters.}
  \item{\dots}{Further parameters passed to \code{segments}.}
}

\details{
This function is used to add an arrow pointing North on a plot made with 
\code{Plotstand}, or \code{plotuspar}. The bearing is defined as in MAESTRA,
where 0 means the X axis points south (and the North arrow thus parallel to 
the X-axis), with 90 degrees the X axis points West, and so forth.

Because I don't know how to tell if the current device is setup by either 
\code{rgl}, \code{Lattice}, or the base graphics \code{plot}, the user needs to
specify what device to add the arrow to. The default is 'rgl'.
}
\value{
  Adds to the current device. Returns error when no device is open.
}
\author{Remko Duursma}

\keyword{ utilities }