###
# rawargos.r
# plots of raw argos accuracy

# set up
library(rgdal)
library(rgeos)
library(sp)

# Winkel Tripel projection for wide view
wtproj <- CRS("+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

source("../../01_helper_functions/tagutils_helperfunctions.r")
source("trubetskoy_colors.r")

# raw locations
loc <- rcsv("../../00_data/zcsync_raw_location.csv")

# test out shapefile
wmap <- rgdal::readOGR("data/ne_10m_land.shp")
wmap_proj <- spTransform(wmap, wtproj)

# project points
coordinates(loc) <- ~Longitude+Latitude
proj4string(loc) <- CRS("+proj=longlat")
loc_proj <- spTransform(loc, wtproj)
locpts <- coordinates(loc_proj)

# lims in robinson proj
lims <- data.frame(lon = c(-130, -30), lat = c(10, 55))
coordinates(lims) <- ~lon+lat
proj4string(lims) <- CRS("+proj=longlat")
lims_proj <- spTransform(lims, wtproj)
limpts <- coordinates(lims_proj)

# plot each location class
uqual <- c("3", "2", "1", "0", "A", "B", "Z")
nqual <- length(uqual)
qualcolors <- trubetskoy_colors(nqual)
cols <- 1:nrow(loc)*NA
for(i in 1:nqual) {
  dese <- loc$Quality == uqual[i]
  cols[dese] <- qualcolors[i]
}



###
### go
###

png("argos_error.png", width = 2400, height = 2400, res = 300)
# A
par(mfrow = c(2, 2), mar = c(3.1, 4.1, .1, 1.1), xpd = TRUE)
plot(wmap_proj, col = "grey55", border = NA, xlim = limpts[, 1], ylim = limpts[, 2], xpd = FALSE)
points(locpts[, 1], locpts[, 2], col = cols, pch = 3)
legend(-13966305,6147766, legend = uqual, col = qualcolors, pch = 3, bty = 'n')
box()
legend("topleft", "(a)", bty = 'n')

# B
hist(loc$Error.Ellipse.orientation, nclass = 50, xlim = c(0, 180), col = "grey75", axes = FALSE, xlab = "", ylab = "count", main = "")
axis(2, las = 1)
axis(1, at = c(0, 45, 90, 135, 180))
legend("topleft", "(b)", bty = 'n')
mtext("Error ellipse orientation (degrees)", side = 1, line = 2.1, cex = .75)

# C
hist(log(loc$Error.Semi.major.axis, base = 10), nclass = 50, xlim = c(1, 6), col = "grey75", axes = FALSE, xlab = "", ylab = "count", main = "")
axis(2, las = 1)
axis(1, at = 1:6, lab = 10^(1:6)/1000)
legend("topleft", "(c)", bty = 'n')
mtext("Error elipse semi-major axis length in km (log scale)", side = 1, line = 2.1, cex = .75)

# D
hist(log(loc$Error.Semi.minor.axis, base = 10), nclass = 25, xlim = c(1, 6), col = "grey75", axes = FALSE, xlab = "", ylab = "count", main = "")
axis(2, las = 1)
axis(1, at = 1:6, lab = 10^(1:6)/1000)
legend("topleft", "(d)", bty = 'n')
mtext("Error elipse semi-minor axis length in km (log scale)", side = 1, line = 2.1, cex = .75)

### close device
dev.off()