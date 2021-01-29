###
# plot_predicted.r
# make a big plot of the foiegras output

# required libraries
library(foieGras)

# helper functions
source("trubetskoy_colors.r")
source("../../01_helper_functions/tagutils_helperfunctions.r")
source("../../01_helper_functions/mapping_functions.r")

# load data
load("../../00_data/zcsync_raw_foiegras_fit.rdata")
loc <- rcsv("../../00_data/zcsync_foiegras.csv")

# projection definition care of Rob Schick
aea <- CRS("+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# add in deploy groups
deploygrp <- data.frame(id = sort(unique(loc$id)), deploygrp = c(rep(1, 5), rep(2, 5), rep(3, 3)))
loc <- merge(loc, deploygrp)

# project points
coordinates(loc) <- ~lon+lat
proj4string(loc) <- CRS("+proj=longlat")
loc_aea <- spTransform(loc, aea)
loc_coords <- as.data.frame(coordinates(loc_aea))

loc <- data.frame(
  id = loc$id, date = loc$date,
  lon = loc$lon, lat = loc$lat,
  x = loc_coords$lon, y = loc_coords$lat,
  deploygrp = loc$deploygrp,
  stringsAsFactors = FALSE
)

locl <- split(loc, loc$deploygrp)


###
### make the plots
###

png("predicted_paths.png", width = 3000, height = 2400, res = 300)
par(mfrow = c(3, 3), mar = c(4.1, 4.1, 1.1, 1.1))
make_basemap()

### first deploy grp
curloc <- locl[[1]]
locl1 <- split(curloc, curloc$id)

# set up the plot
ylim_aea <- c(0, 4.25e+05)
xlim_aea <- c(2e+05, 3.45e+05)
plot_basemap(xlim = xlim_aea, ylim = ylim_aea)
plot_ticks()

# colors
colors1 <- trubetskoy_colors_nogreyscale(19)[1:5]
for(i in 1:length(locl1)) {
  points(locl1[[i]]$x, locl1[[i]]$y, col = colors1[i], pch = 16, cex = .5)
}
legend("bottomright", legend = sapply(locl1, function(l) l$id[1]), col = colors1, pch = 16, bg = rgb(1, 1, 1, .5))
legend("topleft", "(a)", bty = 'n')

### end

### second deploy grp
curloc <- locl[[2]]
locl2 <- split(curloc, curloc$id)

# set up the plot
ylim_aea <- c(1e+05, 2.5e+05)
xlim_aea <- c(2e+05, 3.5e+05)
plot_basemap(xlim = xlim_aea, ylim = ylim_aea, scalebar_x_pos = 1.8e+05, scalebar_y_pos = 1.1e+05)
plot_ticks()

# colors
colors2 <- trubetskoy_colors_nogreyscale(19)[6:10]
for(i in 1:length(locl2)) {
  points(locl2[[i]]$x, locl2[[i]]$y, col = colors2[i], pch = 16)
}
legend("bottomright", legend = sapply(locl2, function(l) l$id[1]), col = colors2, pch = 16, bg = rgb(1, 1, 1, .5))
legend("topleft", "(b)", bty = 'n')
### end

### third deploy grp
curloc <- locl[[3]]
locl3 <- split(curloc, curloc$id)

# set up the plot
ylim_aea <- c(1e+05, 2.5e+05)
xlim_aea <- c(2e+05, 3.5e+05)
plot_basemap(xlim = xlim_aea, ylim = ylim_aea, scalebar_x_pos = 1.8e+05, scalebar_y_pos = 1.1e+05)
plot_ticks()

# colors
colors3 <- trubetskoy_colors_nogreyscale(19)[11:13]
for(i in 1:length(locl3)) {
  points(locl3[[i]]$x, locl3[[i]]$y, col = colors3[i], pch = 16)
}
legend("bottomright", legend = sapply(locl3, function(l) l$id[1]), col = colors3, pch = 16, bg = rgb(1, 1, 1, .5))

legend("topleft", "(c)", bty = 'n')
### end



###
### do longitude first
###

## first group
all1 <- do.call('rbind', locl1)
plot(all1$date, all1$lon, type = 'n', axes = FALSE, xlab = "", ylab = "longitude")

for(i in 1:length(locl1)) {
  points(locl1[[i]]$date, locl1[[i]]$lon, col = colors1[i], pch = 16)
}

# put axis
dd <- sattagutils::dateseq(all1$date)
labs <- format(num2date(dd), "%d-%b")
labs[(1:length(labs) %% 2) == 0] <- NA # only every other
axis(1, at = dd, lab = labs, las = 2)
axis(1, at = dd, lab = NA, tcl = 0.3)
axis(2, las = 1)
axis(2, lab = NA, tcl = 0.3)
box()
legend("topleft", "(d)", bty = 'n', text.col = "black")

## second group
all2 <- do.call('rbind', locl2)
plot(all2$date, all2$lon, type = 'n', axes = FALSE, xlab = "", ylab = "")

for(i in 1:length(locl2)) {
  points(locl2[[i]]$date, locl2[[i]]$lon, col = colors2[i], pch = 16)
}

# put axis
dd <- sattagutils::dateseq(all2$date)
axis(1, at = dd, lab = format(num2date(dd), "%d-%b"), las = 2)
axis(1, at = dd, lab = NA, tcl = 0.3)
axis(2, las = 1)
axis(2, lab = NA, tcl = 0.3)
box()
legend("topleft", "(e)", bty = 'n', text.col = "black")

## third group
all3 <- do.call('rbind', locl3)
plot(all3$date, all3$lon, type = 'n', axes = FALSE, xlab = "", ylab = "")

for(i in 1:length(locl3)) {
  points(locl3[[i]]$date, locl3[[i]]$lon, col = colors3[i], pch = 16)
}

# put axis
dd <- sattagutils::dateseq(all3$date)
axis(1, at = dd, lab = format(num2date(dd), "%d-%b"), las = 2)
axis(1, at = dd, lab = NA, tcl = 0.3)
axis(2, las = 1)
axis(2, lab = NA, tcl = 0.3)
box()
legend("topleft", "(f)", bty = 'n', text.col = "black")
## end longitude


###
### do latitude 
###

## first group
all1 <- do.call('rbind', locl1)
plot(all1$date, all1$lat, type = 'n', axes = FALSE, xlab = "", ylab = "latitude")

for(i in 1:length(locl1)) {
  points(locl1[[i]]$date, locl1[[i]]$lat, col = colors1[i], pch = 16)
}

# put axis
dd <- sattagutils::dateseq(all1$date)
labs <- format(num2date(dd), "%d-%b")
labs[(1:length(labs) %% 2) == 0] <- NA # only every other
axis(1, at = dd, lab = labs, las = 2)
axis(1, at = dd, lab = NA, tcl = 0.3)
axis(2, las = 1)
axis(2, lab = NA, tcl = 0.3)
box()
legend("topleft", "(g)", bty = 'n', text.col = "black")

## second group
all2 <- do.call('rbind', locl2)
plot(all2$date, all2$lat, type = 'n', axes = FALSE, xlab = "", ylab = "")

for(i in 1:length(locl2)) {
  points(locl2[[i]]$date, locl2[[i]]$lat, col = colors2[i], pch = 16)
}

# put axis
dd <- sattagutils::dateseq(all2$date)
axis(1, at = dd, lab = format(num2date(dd), "%d-%b"), las = 2)
axis(1, at = dd, lab = NA, tcl = 0.3)
axis(2, las = 1)
axis(2, lab = NA, tcl = 0.3)
box()
legend("topleft", "(h)", bty = 'n', text.col = "black")

## third group
all3 <- do.call('rbind', locl3)
plot(all3$date, all3$lat, type = 'n', axes = FALSE, xlab = "", ylab = "")

for(i in 1:length(locl3)) {
  points(locl3[[i]]$date, locl3[[i]]$lat, col = colors3[i], pch = 16)
}

# put axis
dd <- sattagutils::dateseq(all3$date)
axis(1, at = dd, lab = format(num2date(dd), "%d-%b"), las = 2)
axis(1, at = dd, lab = NA, tcl = 0.3)
axis(2, las = 1)
axis(2, lab = NA, tcl = 0.3)
box()
legend("topleft", "(i)", bty = 'n', text.col = "black")

## end
dev.off()
