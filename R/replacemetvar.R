replacemetvar <- function(replacevar, newvalues, oldmetfile="met.dat", newmetfile="metNEW.dat"){
	metlines <- readLines(oldmetfile)
	datastart <- grep("DATA START", metlines)
	DATA <- read.table(oldmetfile, skip=datastart, header=FALSE)    
	preamble <- readLines(oldmetfile)[1:datastart]
    namesloc <- grep("columns", tolower(metlines))
    namesloc <- setdiff(namesloc, grep("nocolumns", metlines))
    namesline <- metlines[namesloc]
    sp <- strsplit(namesline, "=")[[1]][2]
    NAMES <- delempty(strsplit(sp, "\t")[[1]])
    NAMES <- gsub("'", "", NAMES)
    NAMES <- do.call("c", strsplit(NAMES, " ", fixed = TRUE))
    names(DATA) <- NAMES
    
	if(nrow(DATA) != length(newvalues) | (is.matrix(DATA) && nrow(DATA) != nrow(newvalues)))
        stop("Length of new data not equal to nr rows in metfile")

    DATA[,replacevar] <- newvalues
	
	writeLines(preamble,newmetfile)
	write.table(DATA, newmetfile, sep=" ",row.names=FALSE,col.names=FALSE,append=TRUE)
} 
