#' Replaces a namelist
#' 
#' @description Replaces the whole namelist in an input file. All parameters in the namelist
#' must be provided, otherwise MAESTRA/MAESPA will likely crash.
#' 
#' 
#' @param namelist Name of the namelist.
#' @param datfile Name of the input file.
#' @param vals A list of values (see example below).
#' @return Nothing is returned. The input file is modified.
#' @author Remko Duursma
#' @seealso \code{\link{replacePAR}}
#' @references See Belinda Medlyn's MAESTRA homepage at:
#' \url{http://www.bio.mq.edu.au/maestra/ }
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' replaceNameList(namelist="aerodyn", 
#'     datfile="trees.dat", vals=list(zht=30,zpd=3,z0ht=0.6))
#' }
#' 
replaceNameList <- function(namelist=NA, datfile=NA, vals=list()){

    
    if(is.na(datfile))stop("Must provide name of the .dat file.")
    
    # Find NAMELIST and end of it ("/")
    datlines <- str_trim(tolower(readLines(datfile)))
	  namelist <- tolower(namelist)
    nmreg <- paste0("&",namelist)
	
    nl_start <- grep(nmreg, datlines)
    
    # Find nearest namelist closer ("/")
    endnml <- grep("^/$", datlines)
    nmllen <- min(endnml[endnml > nl_start] - nl_start)
    nl_end <- nl_start + nmllen
    
    datlines_namelist <- datlines[nl_start:(nl_start + nmllen)]
    
    # Part of original file before and after this namelist.
    if(nl_start > 1){
      pref <- datlines[1:(nl_start-1)]
    } else {
      pref <- ""
    }
    if((nl_end+1) < length(datlines)){
      postf <- datlines[(nl_end+1):length(datlines)]
    } else {
      postf <- ""
    }
    
    # New namelist
    listvals <- c()
    for(i in 1:length(vals)){
      listvals[i] <- paste(names(vals)[i],printme(vals[[i]], "\n"), 
                           sep=" = ")
      newlist <- c(paste0("&",namelist),
                    listvals, "/")
    }
    
    # Rewrite file
    Lines <- c(pref, newlist, postf)
    writeLines(Lines, datfile) 
}

