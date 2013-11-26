#' @title Parse an input file
#' @description Takes an input file for MAESTRA/MAESPA, and reads all namelists into a nested list. 
#' @param fn Filename
#' @return Returns a named list, each element contains a namelist and its parameters.
#' @seealso To read one namelist from a file, see \code{\link{readNameList}}.
#' @export
#' @examples 
#' \dontrun{
#' # Parse a file
#' con <- parseFile("confile.dat")
#' 
#' # Namelists in the file
#' names(con)
#' }
parseFile <- function(fn){
  
  r <- str_trim(tolower(readLines(fn)))
  
  nml <- r[grep("^&",r)]
  nml <- gsub("&","",nml)
  
  l <- list()
  for(i in 1:length(nml)){
    
    l[[i]] <- readNameList(fn, nml[i])
    
  }
  names(l) <- nml
return(l)
}