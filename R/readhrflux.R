`readhrflux` <-
function(filename="hrflux.dat"){
	
	hrlines <- readLines(filename)
	colloc <- grep("Columns",hrlines)

	hrflux <- read.table(filename, skip=colloc,na.strings='NaN')
	names(hrflux) <- delempty(strsplit(delempty(strsplit(hrlines[colloc], "Columns:")[[1]])," ")[[1]])

	hrflux$conttime <- with(hrflux, DOY + HOUR/max(HOUR))
	
	hrflux
}

