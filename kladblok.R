readallPAR <- function(fn){
  
  r <- str_trim(readLines(fn))
  
  nml <- grep("^&",r)
  
  s <- r[nml[1]:length(r)]
  
}


readNameList <- function(fn, namelist){
  
  r <- str_trim(readLines(fn))
  
  nmStart <- grep(paste0("&",namelist), r, ignore.case=TRUE)
  r <- r[nmStart[1]:length(r)]
  r <- r[1:grep("/",r)[1]]
  r <- r[-c(1,length(r))]
  
}
  


s <- "speciesnames = 'Petit' 'Moyen' 'Grand'"
parsePARline <- function(s){

  sp <- strsplit(s, "=")[[1]]
  if(length(sp) != 2)stop("Fatal error in parsePAR.")
  
  parname <- str_trim(sp[1])
  parval <- str_trim(sp[2])
  
  # try splitting

}



