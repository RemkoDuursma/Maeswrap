`mayberead` <-
function(...){

	res <- try(utils::read.table(...))
	if(inherits(res, "try-error")){
		res <- NA
		cat("Error caught - proceeding.\n")
	}
return(res)
}

