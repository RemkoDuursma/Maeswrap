`readPAR` <-
function(datfile, parname, namelist=NA,fail=TRUE){

	# read the file
	dat_lines <- tolower(readLines(datfile))
	parname <- paste("^", tolower(parname),sep="")
	if(!is.na(namelist))namelist <- tolower(namelist)

	# If namelist is not NA, find the parameter within some namelist.
	# (otherwise find the parameter by its name).
	namelist_loc <- 0
	if(!is.na(namelist)){

		nl <- paste("&", namelist, "$", sep="")
		namelist_loc <- grep(nl, trim(dat_lines))
		if(length(namelist_loc)==0){
			if(fail)stop("Can't find namelist\n")
			if(!fail)return(NA)
		}
		
		namelist_end <- NA
		k <- 1
		while(is.na(namelist_end)){
         if(Maeswrap::trim(dat_lines[namelist_loc + k]) == "/")namelist_end <- k
         k <- k + 1
        }
		datlines_namelist <- tolower(dat_lines[namelist_loc:(namelist_loc + namelist_end)])

		# nth element of the namelist
		# if values separated by /n, here separate elements of the vector!
		parloc <- grep(tolower(parname), trim(datlines_namelist)) #+ namelist_loc - 1

		if(length(parloc)==0){
			if(fail)stop(paste("Cannot find",parname,"in",datfile,"in the namelist",namelist,"\n"))
			if(!fail)return(NA)
		}

		# paste everything starting from the parname to end of namelist into a string
		parvalues <- paste(datlines_namelist[parloc:(length(datlines_namelist)-1)], collapse="\t")
		
		# split by "=", and then by "\t"
		s <- strsplit(Maeswrap::trim(parvalues), "=")[[1]][2]
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
		parloc <- grep(paste(parname,"$",sep=""), trim(dat_lines))
		if(length(parloc)==0){
			if(fail)stop(paste("Cannot find",parname,"in",datfile,"\n"))
			if(!fail)return(NA)
		}
		s <- strsplit(Maeswrap::trim(dat_lines[parloc]), "=")[[1]]
		options(warn=-1)
		val <- as.numeric(s[length(s)])
		if(all(is.na(val)))val <- s[length(s)]
		options(warn=0)
	}
	
return(val)
}

