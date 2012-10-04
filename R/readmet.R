`readmet` <-
function(filename="met.dat", nlines=-1){

	metlines <- readLines(filename)
	namesloc <- grep("^columns",tolower(metlines))
	# namesloc <- setdiff(namesloc,grep("nocolumns",metlines))
	
	namesline <- metlines[namesloc]
	datastart <- grep("DATA STARTS", metlines)
	sp <- strsplit(namesline, "=")[[1]][2]
	NAMES <- delempty(trim(strsplit(sp, "\t")[[1]]))
	NAMES <- gsub("'", "", NAMES)
	NAMES <- do.call("c", strsplit(NAMES, " ", fixed=TRUE))
	metdata <- read.table(filename, header=FALSE, skip=datastart, nrows=nlines)
	names(metdata) <- NAMES
	names(metdata)[names(metdata) == "RH%"] <- "RHperc"

return(metdata)
}

