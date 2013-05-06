`readPAR` <-
function(datfile, parname, namelist=NA,fail=TRUE){

	# read the file
	datlines <- str_trim(tolower(readLines(datfile)))
	
  # regexp
  parreg <- paste0("^", tolower(parname), "[[:blank:]]+=")
	
  if(!is.na(namelist))namelist <- tolower(namelist)

	# If namelist is not NA, find the parameter within some namelist.
	# (otherwise find the parameter by its name).
	if(!is.na(namelist)){

		nl <- paste0("&", namelist, "$")
		namelist_loc <- grep(nl, datlines)
		if(length(namelist_loc)==0){
			if(fail)
        stop("Can't find namelist ", namelist)
			else
        return(NA)
		}
		
    # Find nearest namelist closer ("/")
		endnml <- grep("^/$", datlines)
    nmllen <- min(endnml[endnml > namelist_loc] - namelist_loc)
		datlines_namelist <- datlines[namelist_loc:(namelist_loc + nmllen)]
    
  
		# nth element of the namelist
		# if values separated by /n, here separate elements of the vector!
		parloc <- grep(parreg, datlines_namelist) #+ namelist_loc - 1

    if(length(parloc)==0){
			if(fail)
        stop(paste("Cannot find",parname,"in",datfile,
                   "in the namelist",namelist,"\n"))
			else
        return(NA)
		}

    nmlsubs <- datlines_namelist[parloc:(length(datlines_namelist)-1)]
    parl <- grep("=", nmlsubs)
    
    # Include next lines within namelist if they don't have an = in it,
    # which means a \n was added in the file (which is allowed!)
    if(length(parl) > 1)
      nmlpar <- nmlsubs[1:(parl[2]-1)]
    else
      nmlpar <- nmlsubs[parl]
    
    val <- parsePARline(nmlpar)
    
	}

	# only parameter name provided
	if(is.na(namelist)){
    
    parloc <- grep(parreg, datlines)
    
		if(length(parloc)==0){
			if(fail)
        stop("Cannot find ",parname," in ",datfile)
			else
        return(NA)
		}
    
    nmlsubs <- datlines[parloc:(length(datlines)-1)]
    parl <- grep("=", nmlsubs)
    if(length(parl) > 1)nmlpar <- nmlsubs[1:(parl[2]-1)]
    val <- parsePARline(nmlpar)
    
	}
	
return(val)
}

