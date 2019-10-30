#' Replace a weather variable
#'
#' Replaces one (or more) of the weather variables in the met.dat file.
#'
#'
#' @aliases replacemetvar replacemetdata
#' @param replacevar Character. Name(s) of the variable to be replaced.
#' @param newvalues Vector of new values for the weather variable, has to be
#' the same length as the number of records in the met.dat file.
#' @param oldmetfile Default name of the met.dat file that will be modified.
#' @param newmetfile Name of the new met.dat file.
#' @param metdfr Dataframe with met data, to be pasted into a met.dat file.
#' @param columns Optional character string : if the 'Columns' statement in the
#' met.dat file is to be replaced.
#' @param khrs Optional. Number of timesteps per day (by default, read from the
#' met.dat file).
#' @param setdates If TRUE (default), fix start and end date in FORMAT field to cover data added.
#' @return Returns nothing.
#' @author Remko Duursma
#' @keywords utilities
#' @examples
#'
#'
#' \dontrun{
#'
#' #:::1.::: Replace precipitation with random number between 0 and 2.
#' # First find out how many records there are:
#' nrecords <- nrow(readmet("met.dat"))
#'
#' # Make new rain
#' newrain <- runif(nrecords, 0, 2)
#'
#' # And replace
#' replacemetvar("PPT",newrain,"met.dat", "newmet.dat")
#'
#'
#' #:::2.::: Replace multiple weather variables.
#' newtair <- runif(nrecords, 0, 35)
#'
#' # Have to make a matrix of the variables to be replaced:
#' newmat <- matrix(cbind(newrain, newtair),ncol=2)
#'
#' # And give a vector of variable names --in the same order as in the matrix!!--.
#' replacemetvar(c("PPT","TAIR"), newmat, "met.dat", "newmet.dat")
#'
#'
#' }
#'
#' @rdname replacemetvar
#' @export
replacemetvar <- function(replacevar, newvalues, oldmetfile="met.dat", newmetfile="metNEW.dat"){
	metlines <- readLines(oldmetfile)
	datastart <- grep("DATA START", metlines, ignore.case=TRUE)
	DATA <- utils::read.table(oldmetfile, skip=datastart, header=FALSE)
	preamble <- readLines(oldmetfile)[1:datastart]
  namesloc <- grep("columns", metlines, ignore.case=TRUE)
  namesloc <- setdiff(namesloc, grep("nocolumns", metlines, ignore.case=TRUE))
  namesline <- metlines[namesloc]
  sp <- strsplit(namesline, "=")[[1]][2]
  NAMES <- delempty(strsplit(sp, "\t")[[1]])
  NAMES <- stringr::str_trim(gsub("'", "", NAMES))
  NAMES <- do.call("c", strsplit(NAMES, " ", fixed = TRUE))
  names(DATA) <- NAMES

	if(nrow(DATA) != length(newvalues) | (is.matrix(DATA) && nrow(DATA) != nrow(newvalues)))
        stop("Length of new data not equal to nr rows in metfile")

  DATA[,replacevar] <- newvalues

	writeLines(preamble,newmetfile)
	utils::write.table(DATA, newmetfile, sep=" ",row.names=FALSE,col.names=FALSE,append=TRUE)
}


#' @rdname replacemetvar
#' @export
replacemetdata <- function (metdfr,
                            oldmetfile = "met.dat",
                            columns=NULL,
                            newmetfile = oldmetfile,
                            khrs=NA,
                            setdates=TRUE){
  
  metlines <- readLines(oldmetfile)
  datastart <- grep("DATA START", metlines, ignore.case=TRUE)
  
  preamble <- readLines(oldmetfile)[1:datastart]
  
  if(is.na(khrs))
    khrs <- readPAR(oldmetfile,"khrsperday","metformat")
  
  N <- nrow(metdfr)
  if(N %% khrs != 0){
    extralines <- N %% khrs
    metdfr <- metdfr[1:(N-extralines),]
  }
  
  writeLines(preamble, newmetfile)
  
  # set end date based on how many rows of data available.
  if(setdates){
    startdate <- readPAR(oldmetfile,"startdate","metformat")
    startDate <- as.Date(startdate[1], "'%d/%m/%y'")
    if(is.na(startDate)){
      startDate <- as.Date(startdate[1], "%d/%m/%y")
    }
    enddate <- startDate + N/khrs
    replacePAR(newmetfile, "enddate","metformat", format(enddate, "%d/%m/%y"))
  }
  
  replacePAR(newmetfile, "nocolumns","metformat", ncol(metdfr))
  replacePAR(newmetfile, "khrsperday","metformat", khrs)
  
  if(!is.null(columns)){
    replacePAR(newmetfile,"columns","metformat",columns,noquotes=TRUE)
  }
  
  g <- readLines(newmetfile,100)
  g[grep("data start",g,ignore.case=TRUE)] <- "DATA STARTS"
  writeLines(g,newmetfile)
  
  data.table::fwrite(metdfr,newmetfile,sep = " ",col.names = FALSE, append = TRUE)
}


