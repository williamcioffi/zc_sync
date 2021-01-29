###
# mapping_functions.r
# some very specific functions for making my map plots

# libraries
library(rgdal)
library(rgeos)
library(marmap)
library(raster)
library(colorspace)



###
### make tick marks
###

plot_ticks <- function(x_ticklims = c(-90, -60), y_ticklims = c(30, 45), yaxislab = TRUE) {
  # get the actual plotting area because these mapping functions don't always obey
  lims_aea <- data.frame(x = par('usr')[3], y = par('usr')[1])
  coordinates(lims_aea) <- ~ x+y
  proj4string(lims_aea) <- aea
  lims_geo <- spTransform(lims_aea, "+proj=longlat")
  
  # make dataframes for axis tick locations to transform
  xtmp <- seq(x_ticklims[1], x_ticklims[2], by = 0.5)
  lonpts <- data.frame(
    x = xtmp,
    y = rep(min(lims_geo$y), length(xtmp))
  )
  
  ytmp <- seq(y_ticklims[1], y_ticklims[2], by = 0.5)
  latpts <- data.frame(
    x = rep(min(lims_geo$x), length(ytmp)),
    y = ytmp
  )
  
  # make labels with the geographic coordinates
  lonlabs <- paste0(sub("-", "", lonpts$x), "° W")
  lonlabs[round(lonpts$x) != lonpts$x] <- "" # only want major
  
  latlabs <- paste0(latpts$y, "° N")
  latlabs[round(latpts$y) != latpts$y] <- "" # only want major
  
  # transform axis tick positions
  coordinates(lonpts) <- ~x+y
  proj4string(lonpts) <- CRS("+proj=longlat")
  lonpts_aea <- spTransform(lonpts, aea)
  
  coordinates(latpts) <- ~x+y
  proj4string(latpts) <- CRS("+proj=longlat")
  latpts_aea <- spTransform(latpts, aea)
  
  box()
  
  if(yaxislab == FALSE) {
    latlabs <- NA
  }

  # apply the axes
  axis(1, at = coordinates(lonpts_aea)[, 1], labels = NA, tcl = .3)
  axis(1, at = coordinates(lonpts_aea)[, 1], labels = lonlabs, tcl = -.3)
  
  axis(2, at = coordinates(latpts_aea)[, 2], labels = NA, las = 1 , tcl = .3)
  axis(2, at = coordinates(latpts_aea)[, 2], labels = latlabs, las = 1, tcl = -.3)
}



###
### load basemap
###

make_basemap <- function() {
  # get the grid
  hat <- marmap::getNOAA.bathy(lon1 = -78, lon2 = -70, lat1 = 30, lat2 = 40, resolution = 1, keep = TRUE)
  
  # projection
  hat_ras <- marmap::as.raster(hat)
  hat_ras_proj <- projectRaster(hat_ras, crs = aea)
  hat_bathy_proj <<- as.bathy(hat_ras_proj) # global
  
  # load the us coastline
  us_clip <- rgdal::readOGR("../figure_1s_hatterasmap/nos80k_clip/nos80k_clip.shp")
  us_clip_simp <- gSimplify(us_clip, tol = .005)
  us_clip_simp_proj <- spTransform(us_clip_simp, aea)
  us_clip_proj <<- spTransform(us_clip, aea) # global
}



###
### plot basemap
###

plot_basemap <- function(
  xlim, ylim,
  scalebar_x_pos = 100000, scalebar_y_pos = 20000,
  scalebar_len_km = 50, scalebar_lab = "50km"
) {
  waterpal <- colorspace::sequential_hcl(100, c = 0, l = c(0, 85))
  
  # make the plot
  plot(hat_bathy_proj, 
       image = TRUE, land = FALSE,
       shallow = -200, deep = -3000, step = 400, lwd = seq(2, 1, length = 8), col = "grey35",
       bpal = list(c(min(hat_bathy_proj, na.rm = TRUE), 0, waterpal)),
       axes = FALSE,
       ylim = ylim,
       xlim = xlim,
       ylab = "",# "northing",
       xlab = ""# "easting"
  )
  
  # add in the coastline and scalebar
  plot(us_clip_proj, col = "grey30", border = "grey60", add = TRUE, lwd = .05)
  segments(scalebar_x_pos, scalebar_y_pos, scalebar_x_pos + scalebar_len_km*1000, scalebar_y_pos)
  segments(scalebar_x_pos, scalebar_y_pos - 5000, scalebar_x_pos, scalebar_y_pos + 5000)
  segments(scalebar_x_pos + scalebar_len_km*1000, scalebar_y_pos - 5000, scalebar_x_pos + scalebar_len_km*1000, scalebar_y_pos + 5000)
  text(scalebar_x_pos + scalebar_len_km*1000/2, scalebar_y_pos + 5000, scalebar_lab, cex = .75)
}
