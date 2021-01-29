###
# 02_hatterasmap.r
# make a map of tagging locs

# needed libraries
library(marmap)
library(rgdal)
library(rgeos)
library(raster)

# source helper functons
source("../../01_helper_functions/mapping_functions.r")

# projection definition care of Rob Schick
aea <- CRS("+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# load points
dpts <- read.table("taglocations.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

coordinates(dpts) <- ~lon+lat
proj4string(dpts) <- CRS("+proj=longlat")
dpts_proj <- spTransform(dpts, aea)
dpts_df <- cbind(coordinates(dpts_proj), attr(dpts_proj, "data"))

# make the basemap
make_basemap()



###
### make figure
###

png("hatterasmap.png", res = 300, width = 2000, height = 2000)
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  
  plot_basemap(xlim = c(2e+05, 2.25e+05), ylim =  c(0, 3.25e+05))
  plot_ticks()
  points(dpts_df[, 1], dpts_df[, 2], pch = 3, col = rgb(204/255, 85/255, 0), cex = 1)
  
  # legend("topright", legend = c("200m isobath", "1000m isobath"), lwd = c(.5, 3), col = rep("grey35", 2), bty = 'n')
dev.off()
