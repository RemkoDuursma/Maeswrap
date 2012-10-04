`readhrflux` <-
function(filename="hrflux.dat"){
	
	hrlines <- readLines(filename)
	colloc <- grep("Columns",hrlines)

	hrflux <- read.table(filename, skip=colloc,na.strings='NaN')
	names(hrflux) <- delempty(strsplit(delempty(strsplit(hrlines[colloc], "Columns:")[[1]])," ")[[1]])
	 
	 
	#names(hrflux) <- c("DOY", "Tree", "HOUR", "hrPAR", "hrNIR", "hrTHM", "hrPs", "hrRf", 
	#					"hrRmW", "hrLE", "LECAN", "Gscan", "hrH", "TCAN", "VPD")
	hrflux$conttime <- with(hrflux, DOY + HOUR/max(HOUR))
	
	hrflux
}

