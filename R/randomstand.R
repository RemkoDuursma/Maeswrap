#' Generate a simple random stand of trees
#' @description Generates a stand of trees, given a LAI, stocking, and some basic allometry. Very simple implementation that will be expanded (and eventually rolled into Maes*).
#' @export
randomstand <- function(LAI = 2,
                        height = 20,
                        cwcl = 0.8,
                        ALAC = 0.5,
                        stocking = 500,  # ha-1
                        edge = 25,
                        plotsize = c(100,100),
                        crownshape = c("BOX","CONE","PARA","ELIP","CYL"),
                        path = "",
                        maxnotrees=25  # max. number of trees for confile NOTREES (for speed!)
){
  
  
  o <- getwd()
  on.exit(setwd(o))
  if(path != "")setwd(path)
  
  stocking <- stocking / 10000 # convert to m-2
  crownshape <- match.arg(crownshape)
  
  plotsize <- plotsize + 2*edge
  xmax <- plotsize[1]
  ymax <- plotsize[2]
  plotsize <- xmax * ymax
  
  
  ntrees <- floor(stocking * plotsize)
  LAtree <- LAI / stocking
  
  # Random coordinates.
  # Could use a better one from sp package!
  XY <- cbind(runif(ntrees, 0,xmax), runif(ntrees, 0,ymax))
  
  # Crown dimensions, based on total leaf area and width/length ratio of crown.
  AC <- LAtree / ALAC
  cw <- CWfun(crownshape, AC, cwcl)
  cl <- cw / cwcl
  
  if(cl > height)height <- cl
  hcb <- height - cl
  
  # used to set itargets
  whichinplot <- apply(XY, 1, function(x) (x[1] > edge & x[1] < (xmax-edge)) & (x[2] > edge & x[2] < (xmax-edge)))
  
  # set trees.dat parameters
  tf <- "trees.dat"
  replacePAR(tf,"xycoords","xy",XY)
  replacePAR(tf,"notrees","plot",floor(ntrees))
  replacePAR(tf,"values","indivlarea",rep(LAtree,ntrees))
  replacePAR(tf,"values","indivhttrunk",rep(hcb,ntrees))
  replacePAR(tf,"values","indivradx",rep(cw/2,ntrees))
  replacePAR(tf,"values","indivrady",rep(cw/2,ntrees))
  replacePAR(tf,"values","indivhtcrown",rep(cl,ntrees))
  replacePAR(tf,"xmax","plot",xmax)
  replacePAR(tf,"ymax","plot",ymax)
  replacePAR(tf,"x0","plot",0)
  replacePAR(tf,"y0","plot",0)
  
  
  # set confile pars
  cf <- "confile.dat"
  replacePAR(cf, "itargets", "treescon", c(1:ntrees)[whichinplot])
  replacePAR(cf, "notrees", "treescon", min(ntrees,maxnotrees))
  
  
  return(list(cl=cl,cw=cw,AC=AC,hcb=hcb,LAtree=LAtree))  
}

