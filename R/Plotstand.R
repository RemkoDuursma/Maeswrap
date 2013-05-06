Plotstand <- function(treesfile="trees.dat", 
          				    strfile="str.dat",
          					  crownshape=c("cone","ellipsoid","round","halfellipsoid","paraboloid","cylinder"), 
          					  readstrfiles=TRUE,
          					  targethighlight=TRUE,
          					  addNarrow=TRUE, 
                      xyaxes=TRUE,
                      labcex=1,
                      axiscex=1,
                      verbose=FALSE,
                      idate=1,
                      ...){


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
		strfiles <- strfile
	} else {
		strfiles <- readPAR("confile.dat","strfiles","species",fail=FALSE)
		if(all(is.na(strfiles)))
			strfiles <- strfile
		else {
			strfiles <- gsub("'","",strfiles)
		}
		if(!all(file.exists(strfiles)))
			stop("Not all strfiles are in the current working directory.")
	}
	
	if(readstrfiles){
		species <- readPAR("trees.dat","ispecies","speclist",fail=FALSE)
		if(all(is.na(species)))
      species <- rep(1,notrees)
    
		for(i in 1:notrees){
			crownshapes[i] <- tolower(readPAR(strfiles[species[i]],"cshape","canopy"))
	  }
		crownshapes <- gsub("'","",crownshapes)
	
	} else crownshapes[] <- match.arg(crownshape)  
	

  xycoor <- readPAR(treesfile, "xycoords", "xy", fail=FALSE)
  if(all(is.na(xycoor)))stop("XY coordinates must be present in trees.dat file")
  xycoor <- matrix(xycoor,ncol=2,byrow=TRUE)
  X <- xycoor[,1]
  Y <- xycoor[,2]
  
  Bearing <- readPAR(treesfile, "bearing", "plot")
  
  # varname w/o 'indiv' or 'all'!
  readVar <- function(varname, idate=1){
    
    indvar <- paste0("indiv",varname)
    allvar <- paste0("all",varname)
    
    vals <- readPAR(treesfile, "values", indvar,fail=FALSE)
    whichvar <- indvar
    
    if(all(is.na(vals))){
      vals <- rep(readPAR(treesfile, "values", allvar)[1], notrees)
      whichvar <- allvar
    }
      
    # dates?
    ndates <- readPAR(treesfile, "nodates", whichvar)
    if(all(is.na(ndates)) || ndates==1)return(vals)
    
    # else...
    vals <- matrix(vals, ncol=ndates, byrow=TRUE)
    return(vals[,idate])
  }
  
  radx <- readVar("radx", idate) 
  CW <- 2*radx
     
  CL <- readVar("htcrown", idate)
	DBH <- readVar("diam", idate)
	if(max(DBH) > 3)DBH <- 0.01 * DBH
  
  HCB <- readVar("httrunk", idate)
  
  Openstand(treesfile)

  for(i in 1:notrees){
	if(verbose)message("Plotting tree number : ", i)
      plottree(crownshape=crownshapes[i], CL=CL[i], CW=CW[i], 
          HCB=HCB[i], X=X[i], Y=Y[i], dbh=DBH[i], crowncolor=crowncolors[i])
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