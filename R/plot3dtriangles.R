plot3dtriangles <- function(m,...){
	tm <- try(t(geometry::surf.tri(m, geometry::delaunayn(m, options="Pp"))))
	if(!inherits(tm, "try-error"))
		rgl.triangles(m[tm,1], m[tm,2], m[tm,3],...)	
	else
		warning("Problem in surf.tri, tree skipped")
}
