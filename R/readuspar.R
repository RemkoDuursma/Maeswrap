readuspar <- function(){

	dataset <- read.table("uspar.dat", header=TRUE)
	if(ncol(dataset) != 12)stop("Format of uspar.dat has changed\n")
    names(dataset) <- c("day","hour","point","x","y","z","beam","diff","ipar","apar","psus","etus")
	
return(dataset)
}