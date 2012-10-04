plotuspar <- function(what=c("beam","ipar","diff","apar"), dataset=NULL, 
                      day=1, hour=NA, xlim=NULL, ylim=NULL,
					  makepdf=FALSE, outputfile = "apar understorey.pdf", scaleeach=TRUE,
					  addNarrow = TRUE
){

	what <- match.arg(what)
	r <- require(lattice)
    if(!r)stop("Need lattice package\n")
	
	if(addNarrow)Bearing <- try(readPAR("trees.dat", "bearing", "plot"),silent=TRUE)
	if(addNarrow && inherits(Bearing, "try-error")){
		warning("Could not read bearing\n")
		addNarrow <- FALSE
	}
	
	x0 <- try(readPAR("ustorey.dat", "X0", "control"))
	y0 <- try(readPAR("ustorey.dat", "Y0", "control"))
	xmax <- try(readPAR("ustorey.dat", "XMAX", "control"))
	ymax <- try(readPAR("ustorey.dat", "YMAX", "control"))
	if(any(sapply(c(x0,y0,xmax,ymax),inherits,"try-error"))){
		boxdraw <- FALSE
		warning("Could not read x0,y0,xmax and/or ymax.")
	} else boxdraw <- TRUE
	
	if(is.null(dataset)){
        dataset <- readuspar()
    }
    dataset$Z <- dataset[,what]
	nhour <- max(dataset$hour)
	
    if(!is.na(day))
		dataset <- dataset[dataset$day==day,]
	
	if(!is.na(hour))
		dataset <- dataset[dataset$hour==hour,]
		
	nhours <- length(unique(dataset$hour))
	
    z <- list();k <- 1
    for(ihour in unique(dataset$hour)){
      DATA <- subset(dataset, hour == ihour)
      
      if(nhour == 24){
        TIME <- format(strptime(as.character(ihour), format="%H"), format="%H:%M")
      }
      if(nhour == 48){
         HOUR <- as.character(ihour %/% 2)
         MIN <- as.character( 30 * ihour %% 2)
         hourmin <- paste(HOUR,MIN, sep=" ")
         TIME <- format(strptime(hourmin, format="%H %M"), format="%H:%M")
      }
      
	  if(is.null(xlim))xlim <- c(0,max(DATA$x))
	  if(is.null(ylim))ylim <- c(0,max(DATA$y))
	  
	  X0 <- xlim[1] + 0.1*(xlim[2] - xlim[1])
	  Y0 <- ylim[1] + 0.1*(ylim[2] - ylim[1])
	  LEN <- 0.1 * (xlim[2] - xlim[1])
	  
	  if(scaleeach)
		AT <- seq(0,max(DATA$Z),length=50)
	  else
		AT <- seq(0,max(dataset$Z),length=50)
		
	  if(all(AT == 0))next
	  	  
      z[[k]] <- with(DATA, levelplot(Z ~ x*y, 
        main=TIME,
        xlim=xlim,ylim=ylim,
        at=AT,
		panel=function(x,y,...){
			panel.levelplot(x,y,...)
			if(addNarrow)addarrow(X0,Y0,len=LEN,bearing=Bearing,addto="lattice")
			if(boxdraw)lrect(x0,y0,xmax,ymax,border="darkgrey")
		},
        col.regions=grey(seq(0,1,length=50))))
      k <- k + 1
    }
	
	if(makepdf && revchar(substr(revchar(outputfile),1,4)) != ".pdf"){
		outputfile <- paste(outputfile,".pdf",sep="")
	}
    if(makepdf)pdf(outputfile,onefile=TRUE)
    for(i in seq_along(z)){print(z[[i]])}  #;Sys.sleep(0.2)
    if(makepdf)dev.off()

}
