`readdayflux` <-
function(filename="dayflx.dat"){

	daylines <- readLines(filename)
	colloc <- grep("Columns",daylines)
	dayflux <- read.table(filename, skip=colloc)
	names(dayflux) <- delempty(strsplit(delempty(strsplit(daylines[colloc], "Columns:")[[1]])," ")[[1]])
	 
	#names(dayflux) <- c("DOY","Tree","absPAR","absNIR","absTherm","totPs","totRf","netPs","totLE1","totLE2","totH")

dayflux
}

