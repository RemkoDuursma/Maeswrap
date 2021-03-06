#' Reads the value of a parameter in a MAESTRA/MAESPA input file.
#' 
#' @description The `readPAR` function reads the value of any parameter in a namelist
#' in one of the MAESTRA/MAESPA input files.  Also works for other text files
#' that have the FORTRAN namelist input structure.  Optionally specifies in
#' which namelist to look for the parameter.
#' 
#' To read an entire namelist into a list, use the `readNameList`
#' function.
#' 
#' 
#' @aliases readPAR readNameList
#' @param datfile Name of the input file.
#' @param parname Name of the parameter.
#' @param namelist The namelist to look in, otherwise looks in the whole file.
#' @param fail Logical. If TRUE, stops with an error when parameter is not
#' found (if FALSE, returns NA)
#' @return For `readPAR`, either one value, or a vector, depending on how
#' many values are specified for the parameter in the input file.
#' 
#' For `readNameList`, a named list.
#' @author Remko Duursma. Thanks to Andreas Ibrom for reporting a bug.
#' @seealso [replacePAR()], [readNameList()]
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' # Read the number of trees in the plot:
#' readPAR("confile.dat", "notrees", "plot")
#' 
#' # Read the X and Y coordinates:
#' readPAR("confile.dat", "xycoords", "xy")
#' 
#' # Read entire namelist
#' readNameList("trees.dat", "plot")
#' 
#' }
#' 
#' @export
#' @rdname readPAR
readPAR <- function(datfile, parname, namelist=NA,fail=TRUE){

  # Read entire file
  p <- parseFile(datfile)
  
  if(is.na(namelist)){
    # Find parameter
    res <- unlist(unname(lapply(p, "[[", parname)))
  } else {
    
    if(!tolower(namelist) %in% tolower(names(p)) && fail)
      stop("Namelist ",namelist," not found")
    
    res <- p[[namelist]][[parname]]
  }
  
  if(is.null(res) && fail)stop("Parameter not found.")
  if(is.null(res) && !fail)res <- NA
  
  return(res)
}


#' @export
#' @rdname readPAR
readNameList <- function(datfile, namelist){
  
  r <- stringr::str_trim(readLines(datfile))
  
  nmStart <- grep(paste0("&",namelist, "$"), r, ignore.case=TRUE)
  r <- r[nmStart[1]:length(r)]
  r <- r[1:grep("^/$",r)[1]]
  r <- r[-c(1,length(r))]
  r <- delempty(r)
  
  # figure out which elements belong to which parameter.
  parloc <- grep("=",r)

  last <- length(r) - parloc[length(parloc)] + 1
  nlines <- c(diff(parloc),last)
  
  l <- list()
  for(i in 1:length(parloc)){
    ind <- parloc[i]:(parloc[i] + nlines[i] -1)
    l[[i]] <- parsePARline(r[ind])
  }
  
  parnames <- stringr::str_trim(sapply(strsplit(r[parloc],"="), "[", 1))
  names(l) <- tolower(parnames)
  
  return(l)
}


