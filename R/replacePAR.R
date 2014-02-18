#' Replace a parameter value
#' 
#' @description Replaces a parameter value in a MAESTRA/MAESPA input file. Also works for a
#' vector of input values.
#' 
#' If the new parameter value(s) is a vector (or a single value), the values
#' will be placed on a single line in the parameter file. If instead a matrix
#' is provided, each row of the matrix is placed on a separate line.
#' 
#' WARNING : The input file is modified. Make sure to backup your original
#' input files!
#' 
#' @param datfile Name of the input file.
#' @param parname Name of the parameter to replace the value of.
#' @param namelist Optional. In which namelist to look for the parameter.
#' @param newval New value of the parameter. Can be a single value or a vector,
#' or a matrix (see Details).
#' @param noquotes Logical. If FALSE, does print quotes around character
#' values.
#' @return Nothing is returned. The inputfile is modified.
#' @author Remko Duursma
#' @seealso \code{\link{replaceNameList}}, \code{\link{readPAR}}
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' # Replace a parameter with a single value:
#' replacePAR("trees.dat", "notrees", "plot", newval=100)
#' 
#' # Replace a number of values:
#' replacePAR("trees.dat", "xycoords", "xy", newval=c(1,1,2,2,3,3))
#' 
#' # Replace a number of values in a different way : this may be useful in 
#' # more complicated programs,
#' # rr when reading a string from a file (that should be interpreted as a vector).
#' replacePAR("trees.dat", "xycoords", "xy", newval="1 1 2 2 3 3", noquotes=TRUE)
#' 
#' # Replace a parameter so that the new values are placed on multiple rows.
#' # Useful for tree namelists with multiple dates and multiple trees 
#' # (where each row corresponds to a tree, and each column to a date.)
#' m <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, byrow=TRUE)
#' replacePAR("trees.dat", "indivlarea", "values", newval=m)
#' }
#' @export
replacePAR <- function(datfile, parname, namelist=NA, newval, noquotes=FALSE){

	# read the file
	dat_lines <- readLines(datfile)
	parname <- tolower(parname)
	if(!is.na(namelist))namelist <- tolower(namelist)

	# If namelist is not NA, find the parameter within some namelist.
	namelist_loc <- 0
	if(!is.na(namelist)){
		nl <- paste("&", namelist, sep="")
		
		#namelist_loc <- grep(nl, dat_lines)
		# '$' makes sure namelist matches more exactly (reg. exp.).
		namelist_loc <- grep(paste(nl, "$", sep=""), str_trim(tolower(dat_lines)))

		if(length(namelist_loc)==0)stop(paste("Cannot find namelist",toupper(namelist) ))
		
		namelist_end <- NA
		k <- 1
		while(is.na(namelist_end)){
         if(str_trim(dat_lines[namelist_loc + k]) == "/")namelist_end <- k
         k <- k + 1
        }
		datlines_namelist <- dat_lines[namelist_loc:(namelist_loc + namelist_end)]
		
	}

    # Find the parameter.
    if(!is.na(namelist)){
		parloc <- grep(paste("^",parname,sep=""), str_trim(tolower(datlines_namelist))) + namelist_loc - 1
		if(length(parloc) > 1)stop("Multiple entries of ", parname)
	} else {
		parloc <- grep(paste("^",parname,sep=""), str_trim(tolower(dat_lines)))
		if(length(parloc) > 1)stop("Multiple entries of ", parname)
	}
	
	if(!is.na(namelist)){
		if(length(parloc)==0)stop(paste("Cannot find",parname,"in",datfile,"in the namelist",namelist,"\n"))
    } else {
		if(length(parloc)==0)stop(paste("Cannot find",parname,"in",datfile,"\n"))
	}
	
	# Get rid of multiple lines of original parameter, if any:
	if(length(dat_lines) > (parloc+1)){
	tmp <- dat_lines[(parloc+1):length(dat_lines)]
	endnamelist <- grep("/",tmp,fixed=TRUE)[1]
	nextpar <- grep("=",tmp,fixed=TRUE)[1]
        if(!is.na(nextpar) && length(nextpar) > 0){
    		if(nextpar < endnamelist & nextpar > 1){
    			extralines <- (parloc + 1):(parloc + nextpar - 1)
    			dat_lines <- dat_lines[-extralines]
    		}
        }
	}
	
	# Quoting or not? Do not listen to argument when newval is a matrix.
    if(!noquotes && !is.matrix(newval))newval <- printme(newval)
    
    # Store parameter value(s).
    # All in one line (newval is a vector or a single value).
	if(!is.matrix(newval))dat_lines[parloc] <- paste(parname, "=", newval, sep=" ")    
	
    # A number of lines (newval is a matrix).
    if(is.matrix(newval)){
    
        nr <- nrow(newval)
        tmp <- vector(length=nr)
        tmp[1] <- paste(parname, "=", paste(newval[1,],collapse=" "), sep=" ")  
        for(i in 2:nr)tmp[i] <- paste(newval[i,],collapse=" ")
        N <- length(dat_lines)
        dat_lines <- c(dat_lines[1:(parloc-1)], tmp, dat_lines[(parloc+1):N]) 
    }
    
	# Write all lines to file:
    writeLines(dat_lines, datfile)
}
