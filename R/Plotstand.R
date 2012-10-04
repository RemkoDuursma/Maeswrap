Plotstand <- function(treesfile="trees.dat", 
				      strfile="str.dat",
					  crownshape=c("cone","ellipsoid","round","halfellipsoid","paraboloid","cylinder"), 
					  readstrfiles=TRUE,
					  targethighlight=TRUE,
					  addNarrow=TRUE, xyaxes=TRUE,labcex=1,axiscex=1,verbose=FALSE,...){


	notrees <- readPAR(treesfile, "notrees", "plot")
	
	crownshapes <- rep(NA,notrees)
	
	haveconfile <- file.exists("confile.dat")
	
	if(targethighlight & !haveconfile){
		warning("No confile.dat found - target trees not highlighted")
		targethighlight <- FALSE
		crowncolors <- rep("forestgreen",notrees)
	}
	
	if(targethighlight){
		crowncolors <- rep("forestgreen",notrees)
		itargets <- readPAR("confile.dat", "itargets", "treescon", fail=FALSE)
		if(all(is.na(itargets))){
			warning("itargets not read in confile.dat")
		} else crowncolors[itargets] <- "red"
	}
	
	if(!haveconfile){
		# warning("Guessing str file is str.dat\n")
		strfiles <- strfile
	} else {
		strfiles <- readPAR("confile.dat","strfiles","species",fail=FALSE)
		if(all(is.na(strfiles)))
			strfiles <- strfile
		else {
			# x <- strsplit(strfiles," ")
			strfiles <- gsub("'","",strfiles) #delempty(x[[1]]))
		}
		if(!all(file.exists(strfiles)))
			stop("Not all strfiles are in the current working directory.")
	}
	
	if(readstrfiles){
		species <- readPAR("trees.dat","ispecies","speclist",fail=FALSE)
		if(all(is.na(species))){
            species <- rep(1,notrees)
        }
        #stop("Species namelist not found!")
		for(i in 1:notrees){
			crownshapes[i] <- tolower(readPAR(strfiles[species[i]],"cshape","canopy"))
		}
		crownshapes <- gsub("'","",crownshapes)
	
	} else crownshapes[] <- match.arg(crownshape)  
	
    xycoor <- try(readPAR(treesfile, "xycoords", "xy"),silent=TRUE)
	if(inherits(xycoor, "try-error"))stop("XY coordinates must be present in trees.dat file")
    X <- xycoor[seq(1,length(xycoor),by=2)]
    Y <- xycoor[seq(2,length(xycoor),by=2)]
    
	Bearing <- readPAR(treesfile, "bearing", "plot")
	    
    radx <- try(readPAR(treesfile, "values", "indivradx"),silent=TRUE)
    if(inherits(radx, "try-error"))radx <- rep(readPAR(treesfile, "values", "allradx")[1], notrees)
    CW <- 2 * radx

    CL <- try(readPAR(treesfile, "values", "indivhtcrown"),silent=TRUE)
    if(inherits(CL, "try-error"))CL <- rep(readPAR(treesfile, "values", "allhtcrown")[1], notrees)
    
    DBH <- try(readPAR(treesfile, "values", "indivdiam"),silent=TRUE)
    if(inherits(DBH, "try-error"))DBH <- rep(readPAR(treesfile, "values", "alldiam")[1], notrees)
    if(max(DBH) > 3)DBH <- 0.01 * DBH

    HCB <- try(readPAR(treesfile, "values", "indivhttrunk"),silent=TRUE)
    if(inherits(HCB, "try-error"))HCB <- rep(readPAR(treesfile, "values", "allhttrunk")[1], notrees)

	Openstand(treesfile)
	
    for(i in 1:notrees){
		if(verbose)message("Plotting tree number : ", i)
        plottree(crownshape=crownshapes[i], CL=CL[i], CW=CW[i], 
            HCB=HCB[i], X=X[i], Y=Y[i], dbh=DBH[i], crowncolor=crowncolors[i]) #,...)
    }
	if(addNarrow){
		X0 <- readPAR(treesfile,"x0","plot", fail=FALSE)
		if(is.na(X0))X0 <- 0
		Y0 <- readPAR(treesfile,"y0","plot",fail=FALSE)
		if(is.na(Y0))Y0 <- 0
		
		Xmax <- readPAR(treesfile,"xmax","plot")
		Ymax <- readPAR(treesfile,"ymax","plot")
		addarrow(x0=X0 + 0.1*Xmax,y0=Y0 + 0.1*Ymax,
			 len=0.1*(Ymax-X0),bearing=Bearing)
   }
	if(xyaxes){
		par3d(cex=axiscex)
		axes3d(c('x-','y-'))
		par3d(cex=labcex)
		title3d(xlab="X",ylab="Y")
	}
   
}