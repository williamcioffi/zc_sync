####################
### FILE NOT RUN ###
####################
# 
# This file depends on raw unfiltered data not present in this repository and
# therefore will not run properly, but is included here to present the methods
# used to generate position estimates.
#
# The raw data are not included because they contain experimental treatments
# that are truncated out of the present analysis, but all truncated data
# generated in 04_prepdata_NOTRUN.r are included here. For access to the
# original data please email wrc14@duke.edu.
#



###
# 03_execfoiegras_NOTRUN.r
# generate foiegras positions for sat tags

# required libraries
library(foieGras) # v 0.2.2
library(sf)
library(sp)

# source helper functions
source("01_helper_functions/tagutils_helperfunctions.R")

# prep location file
# loc <- rcsv("00_data/zcsync_raw_location.csv")

# retain all location classes except Z (invalid)
loc <- loc[which(loc$Quality %in% c("0", "1", "2", "3", "A", "B")), ]

# retain only Zcs
loc <- loc[grep("Zc", loc$DeployID), ]

# remove fixes from when tag turned on early (before deployment) on ZcTag065 and ZcTag063. 
# Also remove some obviously impossible points that aren't filtered out automatically

loc <- loc[-which(loc$Longitude < -90 | loc$Longitude > -50), ]

# drop extra columns
loc <- loc[, c(
  'DeployID',
  'Date',
  'Quality',
  'Latitude',
  'Longitude',
  'Error.Semi.major.axis',
  'Error.Semi.minor.axis',
  'Error.Ellipse.orientation'
)]

# convert dates for foieGras
loc$Date <- as.POSIXct(loc$Date, origin = "1970-01-01", tz = "UTC")

# add in tagging locations with 500 meter error and quality 3.
deploy_locations <- rcsv("00_data/deploy_locations.csv")
deploy_locations$Date <- as.POSIXct(paste(deploy_locations$Time, deploy_locations$Day), tz = "UTC", format = "%H:%M %Y-%m-%d")

loc <- rbind(
  loc,
  data.frame(
    DeployID = deploy_locations$DeployID,
    Date = deploy_locations$Date, 
    Quality = 3, 
    Latitude = deploy_locations$Latitude,
    Longitude = deploy_locations$Longitude,
    Error.Semi.major.axis = 500,
    Error.Semi.minor.axis = 500,
    Error.Ellipse.orientation = 0
  )
)

# sort first by Date within DeployID
oo <- order(loc$Date)
oo2 <- order(loc$DeployID[oo])

loc <- loc[oo[oo2], ]

# rename columns to foieGras::fit_ssm expectations
movedat <- data.frame(
  id = loc$DeployID,
  date = loc$Date,
  lc = loc$Quality,
  lon = loc$Longitude,
  lat = loc$Latitude,
  smaj = loc$Error.Semi.major.axis,
  smin = loc$Error.Semi.minor.axis,
  eor = loc$Error.Ellipse.orientation
)

# remove any NAs
dese <- !is.na(movedat$smaj)
movedat <- movedat[dese, ]

###
### run the model
###

# make prediction times every 6 hours.
uids <- unique(movedat$id)
nids <- length(uids)

predt_list <- vector(mode = "list", length = nids)

for(i in 1:nids) {
  # make our output times
  dd <- as.numeric(movedat$date[movedat$id == uids[i]])
  predtime <- seq(
    ceiling(min(dd)/(6*60*60))*(6*60*60), 
    floor(  max(dd)/(6*60*60))*(6*60*60),
    by = 6*60*60
  )
  predtime <- as.POSIXct(predtime, tz = "UTC", origin = "1970-01-01")
  predt_list[[i]] <- data.frame(id = uids[i], date = predtime)
}

predtime_df <- do.call('rbind', predt_list)

# fit
fit <- foieGras::fit_ssm(
  movedat, 
  model = "crw", 
  vmax = 10, # 10 m/s for the sda filter in argosfilter
  # ang is set to -1 by default so it is just a speed filter in this case
  time.step = predtime_df
)

# save the fit
# save(fit, file = "00_data/zcsync_raw_foiegras_fit.RData")

# extract a table of positions
foiegras_pred_list <- lapply(fit$ssm, '[[', 'predicted')

# convert to geographic coords
for(i in 1:length(foiegras_pred_list)) {
  cur <- foiegras_pred_list[[i]]
  # get geo coords and add them back to the dataframe
  cur_geo <- sf::st_transform(cur, sp::CRS("+proj=longlat"))
  lonlat <- do.call('rbind', sf::st_geometry(cur_geo))
  colnames(lonlat) <- c("lon", "lat")
  cur_geo_df <- as.data.frame(cur_geo)
  cur_geo_df <- cbind(cur_geo_df, lonlat)
  
  # remove the old geometry column
  cur_geo_df <- cur_geo_df[, colnames(cur_geo_df) != "geometry"]
  
  # convert datetime to numeric
  cur_geo_df$date <- as.numeric(cur_geo_df$date)
  
  # replace in list
  foiegras_pred_list[[i]] <- cur_geo_df
}

# flatten list
foiegras_pred <- do.call('rbind', foiegras_pred_list)

# save the flattened predictions
# wcsv(foiegras_pred, "00_data/zcsync_raw_foiegras_predicted.csv")
