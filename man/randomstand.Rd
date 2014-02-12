\name{randomstand}
\alias{randomstand}
\title{Generate a simple random stand of trees}
\usage{
randomstand(LAI = 2, height = 20, cwcl = 0.8, ALAC = 0.5,
  stocking = 500, edge = 25, plotsize = c(100, 100),
  crownshape = c("BOX", "CONE", "PARA", "ELIP", "CYL"), path = "",
  maxnotrees = 25)
}
\description{
Generates a stand of trees, given a LAI, stocking, and some
basic allometry. Very simple implementation that will be
expanded (and eventually rolled into Maes*).
}

