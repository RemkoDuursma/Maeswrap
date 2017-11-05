

# The Maeswrap R package

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Maeswrap)](https://cran.r-project.org/package=Maeswrap) [![](https://cranlogs.r-pkg.org/badges/grand-total/Maeswrap)](https://CRAN.R-project.org/package=Maeswrap) [![Bitbucket Issues](https://img.shields.io/bitbucket/issues/remkoduursma/Maeswrap.svg)](https://img.shields.io/bitbucket/issues/remkoduursma/Maeswrap.svg) [![Travis-CI Build Status](https://travis-ci.org/RemkoDuursma/Maeswrap.svg?branch=master)](https://travis-ci.org/RemkoDuursma/Maeswrap.svg?branch=master)

This package is a collection of tools to work with the [Maespa model](maespa.github.io).

Main functions:

- `readPAR` : read a parameter value from an input file into R
- `replacePAR` : replace a parameter value in an input file
- `replaceNameList` : replace an entire namelist, with a list of values
- `parseFile` : read an entire input file into a nested list
- `PlotStand` : plot the stand in 3D (requires the `rgl` package)
- `readdayflux` : read the daily output file into a dataframe (see also `readwatbal`, `readhrflux`)
- `replacemetdata` : replace the met data with a dataframe of values
- `runmaespa` : a simple wrapper to run the model, read output into dataframes


## Installation

The package is hosted on CRAN, so you can do

```
install.packages("Maeswrap")
```

or install the development version via:

```
devtools::install_github("RemkoDuursma/Maeswrap")
```
