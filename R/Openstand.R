Openstand <- function(treesfile="trees.dat"){

    options(show.error.messages=FALSE)
    x0 <- try(readPAR(treesfile,"x0","plot"))
    y0 <- try(readPAR(treesfile,"y0","plot"))
    if(inherits(x0, "try-error"))x0 <- 0
    if(inherits(y0, "try-error"))y0 <- 0
    options(show.error.messages=TRUE)
    
    xmax <- readPAR(treesfile,"xmax","plot")
    ymax <- readPAR(treesfile,"ymax","plot")
    
	M <- matrix(c(x0,y0,0,
			      xmax,y0,0,
				  xmax,ymax,0,
				  x0,ymax,0,
				  x0,y0,0), ncol=3, byrow=TRUE)
	lines3d(M, col="darkgrey")
    
}
