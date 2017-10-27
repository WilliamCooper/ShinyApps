## given geometric altitude, finds the geopotential height
geoPH <- function (z, latitude) {
	gzero <- 9.80665
	g1 <- 0.001931851
  g2 <- 0.006694380
	a2 <- 3.086e-6
	ge <- 9.780327
	DG <- 0  ## for now, neglect height of the geoid
	sin2lat <- sin (latitude * pi/180)^2
	geopot <- ge * ((1+g1*sin2lat) / sqrt(1 - g2*sin2lat))*z - 
		       a2 * (z^2 / 2 + z*DG)
	geopht <- geopot / gzero
	return (geopht)
}

geomH <- function (geopht, latitude) {
	fng <- function (z) {
	return (geoPH(z, latitude) - geopht)
	}
	return(nleqslv(geopht, fng)$x)
}


