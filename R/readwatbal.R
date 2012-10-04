readwatbal <- function(filename="watbal.dat"){

	watlines <- readLines(filename)
	colloc <- grep("Columns",watlines)
	namesline <- watlines[colloc]
	NAMES <- delempty(trim(strsplit(strsplit(namesline, ":")[[1]][2], " ")[[1]]))
	watbal <- read.table(filename, header=FALSE, na.strings="-999.0000", skip=colloc)
	names(watbal) <- NAMES
	
return(watbal)
}