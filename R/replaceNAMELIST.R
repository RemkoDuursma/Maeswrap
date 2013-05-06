`replaceNAMELIST` <-
function(namelist=NA, 
		 datfile=NA, 
		 vals=list()){
    
    if(is.na(datfile))datfile <- file.choose()
    
    # Find NAMELIST and end of it ("/")
    datfile_lines <- tolower(readLines(datfile))
	namelist <- tolower(namelist)
	
    namelist_loc <- length(datfile_lines)
    for(i in 1:length(datfile_lines))if(str_trim(datfile_lines[i]) == paste("&",namelist,sep=""))namelist_loc <- i
        
    namelist_end <- NA
    k <- namelist_loc
    if(!(namelist_loc == length(datfile_lines))){
        while(is.na(namelist_end)){
         if(str_trim(datfile_lines[k]) == "/")namelist_end <- k
         k <- k + 1
        }
        # Part of original file before and after this namelist.
        datfile_before <- datfile_lines[1:(namelist_loc-1)]
		
		if((namelist_end+1) < length(datfile_lines)){
			datfile_after <- datfile_lines[(namelist_end+1):length(datfile_lines)]
		} else {
			datfile_after <- ""
		}
		
    } else {    # if namelist not found, it will be put at end of file.
        datfile_before <- datfile_lines
        datfile_after <- ""
    }

    # New namelist
    listvals <- c()
    for(i in 1:length(vals))listvals[i] <- paste(names(vals)[i],printme(vals[[i]], "\n"), sep=" = ")
    newlist <- c(   paste("&",namelist,sep=""),
                    listvals,   
                    "/"
                    )
					
    # Rewrite file
    Lines <- c(datfile_before, newlist, datfile_after)
    nrlines <- length(Lines)
    write(Lines[1], file=datfile)
    for (i in 2:nrlines) {
	   write(Lines[i], file=datfile, append=TRUE)
    }
    write("\n", file=datfile, append=TRUE)
                    
}

