`replacePAR` <-
function(datfile, parname, namelist=NA, newval, noquotes=FALSE){

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
