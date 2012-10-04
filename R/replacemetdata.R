replacemetdata <- function (metdfr, oldmetfile = "met.dat", 
	columns=NA,
	newmetfile = "met.dat", 
	khrs=NA) 
{
    metlines <- readLines(oldmetfile)
    datastart <- grep("DATA START", metlines)
    #DATA <- read.table(oldmetfile, skip = datastart, header = FALSE)
    preamble <- readLines(oldmetfile)[1:datastart]
	
	if(is.na(khrs))
		khrs <- readPAR("met.dat","khrs","metformat")
	else
		replacePAR("met.dat", "khrs","metformat", khrs)	
	
	startdate <- readPAR("met.dat","startdate","metformat")
	startdate <- as.Date(startdate[1], "'%d/%m/%y'")
	if(is.na(startdate))
		startdate <- as.Date(startdate[1], "%d/%m/%y")

	N <- nrow(metdfr)
	if(N %% khrs != 0){
		extralines <- N %% khrs
		metdfr <- metdfr[1:(N-extralines),]
	}
	enddate <- startdate + N/khrs
	replacePAR("met.dat", "enddate","metformat", format(enddate, "%d/%m/%y"))	
	replacePAR("met.dat", "nocolumns","metformat", ncol(metdfr))	
		
	if(!is.na(columns))
		replacePAR("met.dat","columns","metformat",columns,noquotes=TRUE)	
		
    writeLines(preamble, newmetfile)
    write.table(metdfr, newmetfile, sep = " ", row.names = FALSE, 
        col.names = FALSE, append = TRUE)
}

