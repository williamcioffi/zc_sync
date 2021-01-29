###
# usmap_inset.r
# ok looking inset map

# libs
library(rgdal)
library(rgeos)
library(sp)

# Winkel Tripel
wtproj <- CRS("+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

source("../../code/01_helper_functions/tagutils_helperfunctions.r")
source("trubetskoy_colors.r")

# test out shapefile
wmap <- rgdal::readOGR("data/ne_10m_land.shp")
wmap_proj <- spTransform(wmap, wtproj)

# lims projected
lims <- data.frame(lon = c(-100, -30), lat = c(30, 45))
coordinates(lims) <- ~lon+lat
proj4string(lims) <- CRS("+proj=longlat")
lims_proj <- spTransform(lims, wtproj)
limpts <- coordinates(lims_proj)



###
### open device
###

png("usmap_inset.png", width = 2400, height = 2400, res = 300)
# A
plot(wmap_proj, col = "grey55", border = NA, xlim = limpts[, 1], ylim = limpts[, 2], xpd = FALSE)

miniextent <- data.frame(x = c(-77.5, -73.8), y = c(34, 36.8))
coordinates(miniextent) <- ~x+y
proj4string(miniextent) <- CRS("+proj=longlat")
miniextent_wt <- spTransform(miniextent, wtproj)
miniextent <- coordinates(miniextent_wt)

xx <- c(miniextent[1, 1], miniextent[2, 1], miniextent[2, 1], miniextent[1, 1], miniextent[1, 1])
yy <- c(miniextent[1, 2], miniextent[1, 2], miniextent[2, 2], miniextent[2, 2], miniextent[1, 2])

lines(xx, yy)
box()

### close device
dev.off()