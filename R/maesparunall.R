`maesparunall` <-
function(whichrows=NA, runfile=NA, whichcols=NA, quiet=FALSE, extrafiles="", ...){

if(is.na(runfile))runfile <- file.choose()

# Run all rows of the input runfile, or those provided:
inputdat <- read.csv(runfile)
if(all(is.na(whichrows)))whichrows <- 1:nrow(inputdat)

# Lists of results:
returnlist <- list()

# Loop through rows:
for(i in 1:length(whichrows)){
runmaespa(whichrow=whichrows[i], whichcols=whichcols, runfile=runfile, ...)

returnlist$dayresult[[i]] <- readdayflux()
returnlist$hrresult[[i]] <- readhrflux()
if(file.exists("watbal.dat"))returnlist$watbal[[i]] <- readwatbal()

for(j in 1:length(extrafiles)){

	if(!file.exists(extrafiles[j]))next
	if(extrafiles[j]=="watbal.dat")next
	dat <- read.table(extrafiles[j])
	datname <- gsub(".[a-z]*$", "", extrafiles[j])
	returnlist[[datname]][[i]] <- dat
}
    
if(!quiet)cat("Row", i, "out of", length(whichrows), "completed\n")
}

return(returnlist)
}

