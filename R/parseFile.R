parseFile <- function(fn){
  
  r <- str_trim(tolower(readLines(fn)))
  
  nml <- r[grep("^&",r)]
  nml <- gsub("&","",nml)
  
  l <- list()
  for(i in 1:length(nml)){
    
    l[[i]] <- readNameList(fn, nml[i])
    
  }
  names(l) <- nml
return(l)
}