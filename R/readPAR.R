`readPAR` <-
function(datfile, parname, namelist=NA,fail=TRUE){

	# read the file
	dat_lines <- str_trim(tolower(readLines(datfile)))
	parname <- paste0("^", tolower(parname))
	if(!is.na(namelist))namelist <- tolower(namelist)

	# If namelist is not NA, find the parameter within some namelist.
	# (otherwise find the parameter by its name).
	namelist_loc <- 0
	if(!is.na(namelist)){

		nl <- paste0("&", namelist, "$")
		namelist_loc <- grep(nl, dat_lines)
		if(length(namelist_loc)==0){
			if(fail)
        stop("Can't find namelist\n")
			else
        return(NA)
		}
		
    # Find nearest namelist closer ("/")
		endnml <- grep("^/$", dat_lines)
    nmllen <- min(endnml[endnml > namelist_loc] - namelist_loc)
		datlines_namelist <- dat_lines[namelist_loc:(namelist_loc + nmllen)]
    
  
		# nth element of the namelist
		# if values separated by /n, here separate elements of the vector!
		parloc <- grep(parname, datlines_namelist) #+ namelist_loc - 1

    if(length(parloc)==0){
			if(fail)
        stop(paste("Cannot find",parname,"in",datfile,"in the namelist",namelist,"\n"))
			else
        return(NA)
		}

		# paste everything starting from the parname to end of namelist into a string
		parvalues <- paste(datlines_namelist[parloc:(length(datlines_namelist)-1)], collapse="\t")
		
		# split by "=", and then by "\t"
		s <- strsplit(str_trim(parvalues), "=")[[1]][2]
		s2 <- delempty(strsplit(s, "\t")[[1]])
		
		# Further splitting by " " for values (partially) in one row.
		s2 <- delempty(unlist(strsplit(s2, " ")))
	
		options(warn=-1)
		val <- as.numeric(s2)
		if(all(is.na(val)))val <- s2
		val <- val[!is.na(val)]
		options(warn=0)
	}

	# only parameter name provided
	if(is.na(namelist)){
    
    parLocs <- grep("=",dat_lines)
    parNames <- strsplit(dat_lines[parLocs],"=")
    parNames <- str_trim(sapply(parNames, "[", 1))
    
    parloc <- grep(paste0(parname,"$"), parNames)
    
		parloc <- grep(parname, dat_lines)
		if(length(parloc)==0){
			if(fail)
        stop("Cannot find ",parname," in ",datfile)
			else
        return(NA)
		}
    
    # convert to original line number
    parloc <- parLocs[parloc]
    
		s <- strsplit(dat_lines[parloc], "=")[[1]]
		options(warn=-1)
		val <- as.numeric(s[length(s)])
		if(all(is.na(val)))val <- s[length(s)]
		options(warn=0)
	}
	
return(val)
}

