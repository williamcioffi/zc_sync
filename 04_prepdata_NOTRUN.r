####################
### FILE NOT RUN ###
####################
# 
# This file depends on raw unfiltered data not present in this repository and
# therefore will not run properly, but is included here to present the methods
# used to filter the data for further analysis.
#
# The raw data are not included because they contain experimental treatments
# that are truncated out of the present analysis, but all truncated data
# are included here. For access to the original data please email 
# wrc14@duke.edu.
#



###
# 04_prepdata_NOTRUN.r
# clip data to CEEs or depth sensor fail

# source helper functions
source("01_helper_functions/tagutils_helperfunctions.r")
source("01_helper_functions/findgaps.r")

# constants
MIN_OVERLAP_DAYS <- 1 # min overlap for a dyad in days to be included
CUT_OFF_NUM <- 2 # cut off after this many status message depths below +/- CUT_OFF_DEPTH_THRESHOLD
CUT_OFF_DEPTH_THRESHOLD <- 10 # meters

# load data
# beh <- rcsv("00_data/zcsync_raw_behavior.csv")
# sta <- rcsv("00_data/zcsync_raw_status.csv")
# loc <- rcsv("00_data/zcsync_raw_foiegras_predicted.csv")

# make into lists by tag
stal <- split(sta, sta$DeployID)
behl <- split(beh, beh$DeployID)
locl <- split(loc, loc$id)

# if the very first event is 'surface' then drop it because with the > 33 min
# requirement then the first surf on a tag will not be comparable to
# subsequent surfs which are typically entire IDDIs.
for(i in 1:length(behl)) {
  cur <- behl[[i]]
  
  if(cur$What[2] == "Surface") {
    cur <- cur[-2, ]
    cur$Start[1] <- cur$Start[2]
    behl[[i]] <- cur
  }
}


# filter for pressure sensor drift
status_based_cutoff <- vector()
startstart <- vector()
end_or_fail <- vector()

for(i in 1:length(stal)) {
	cursta <- stal[[i]]
	curbeh <- behl[[i]]

	depover10 <- abs(cursta$Depth[!is.na(cursta$Depth)]) > CUT_OFF_DEPTH_THRESHOLD
	numpastthresh <- length(which(depover10))
	
	startstart[i] <- curbeh[1, ]$Start
	
	if(numpastthresh >= CUT_OFF_NUM) {
		lastgood <- min(which(depover10)) - 1
		if(lastgood > 0) {
			status_based_cutoff[i] <- cursta$Received[lastgood]
			end_or_fail[i] <- "pressure sensor flagged"
		} else {
			status_based_cutoff[i] <- NA # never got a good status message
		}
	} else {
		status_based_cutoff[i] <- cursta$Received[nrow(cursta)]
		end_or_fail[i] <- "last status message"
	}
	
	status_withdeps <- which(!is.na(cursta$Depth))
	status_en <- cursta$Received[max(status_withdeps)]
	behavi_en <- curbeh$End[nrow(curbeh)]
	plot_en <- max(status_en, behavi_en)
}

# tag length unclipped
taglen <- lapply(behl, function(l) {
	(l$End[nrow(l)] - l$Start[1]) / 60 / 60 / 24
}) 

# predicted tag length
taglen_clipped_predicted <- (status_based_cutoff - startstart) / 60 / 60 / 24

# propagate the tag names
names(status_based_cutoff) <- names(stal)

# make a df of relevant information
endtimesdf <- data.frame(DeployID = names(stal), faildate = status_based_cutoff, reason = end_or_fail, stringsAsFactors = FALSE)

###
### calculate cut offs
###

# custom cut off for zc55 at first gap b/c of unknown tag failure
# given some recent failures, I believe the tag may have started
# resetting intermittantly at this point producing unreliable data.
endtimesdf[endtimesdf$DeployID == "ZcTag055", 'faildate'] <- findgaps2(behl[['ZcTag055']])$gap_st[1]
endtimesdf[endtimesdf$DeployID == "ZcTag055", 'reason'] <- "unknown onboard failure"

# put the start dates in there
endtimesdf[, 'start_date'] <- sapply(behl, function(l) min(l$Start))
endtimesdf[, 'end_date'] <- sapply(behl, function(l) max(l$End))

# exposure times
# source("00_data/exposure_times.r")

# clip to exposure
cee1_cut <- endtimesdf$start_date < cee1 & endtimesdf$end_date > cee1
cee2_cut <- endtimesdf$start_date > cee1 & endtimesdf$start_date < cee2 & endtimesdf$end_date > cee2

endtimesdf[, 'cee_clip'] <- cee1
endtimesdf$cee_clip[cee1_cut] <- cee1
endtimesdf$cee_clip[cee2_cut] <- cee2
endtimesdf$cee_clip[!cee1_cut & !cee2_cut] <- NA

# take a look
endtimesdf
datebeforecee <- endtimesdf[, 'faildate'] < endtimesdf[, 'cee_clip']
cbind(endtimesdf[, ], datebeforecee)

###
### apply cut offs
###

# first priority is to clip to the cee
# only if cee_clip is NA will it clip to fail date.
# this works for 54-58
# it works for 66 because the faildate is after the cee_clip
# this does not work for 67 which has a faildate before cee_clip
# but this is the desired behavior because I want to keep 67 
# in the analysis despite pressure sensor failure
# this is because this tag has a known failure mode that should not
# impact analyses using the start and end time of dives and surfs.
# it is removed from any analysis of depths.

for(i in 1:length(behl)) {
	cur <- behl[[i]]
	clip <- endtimesdf$cee_clip[i]
	if(is.na(clip)) clip <- endtimesdf$faildate[i]
	
	behl[[i]] <- cur[cur$End <= clip, ]
}

# tag length in days
taglen_clipped <- lapply(behl, function(l) {
	(l$End[nrow(l)] - l$Start[1]) / 60 / 60 / 24
}) 

# take a look
cbind(taglen, taglen_clipped_predicted, taglen_clipped)
behl <- behl[taglen_clipped >= MIN_OVERLAP_DAYS]
beh <- do.call('rbind', behl)

# add a couple of columns
beh[, 'DepthMean'] <- rowMeans(beh[, c('DepthMax', 'DepthMin')])
beh[, 'DepthError'] <- (beh$DepthMax - beh$DepthMin) / 2

###
### clip positions on location data
###

# at this point the only reason I want to clip positions is to the end of the
# experiment and not for any pressure transducer failures given that they 
# should not impact positional data.

for(i in 1:length(locl)) {
	cur_loc <- locl[[i]]	
	clip <- endtimesdf$cee_clip[i]

	if(!is.na(clip)) {	
		dese <- (cur_loc$date <= clip)
		locl[[i]] <- cur_loc[dese, ]
	}
}

# give the location length
loctaglen_clipped <- lapply(locl, function(l) {
	(l$date[nrow(l)] - l$date[1]) / 60 / 60 / 24
})

# take a look
cbind(taglen, taglen_clipped, loctaglen_clipped)

# remove ZcTag065 which has less than one day of data
locl <- locl[loctaglen_clipped > MIN_OVERLAP_DAYS]

# bind back up into a data frame
loc <- do.call('rbind', locl)
# plot(loc$lon, loc$lat, col = as.factor(loc$id))

### output prepared data
# wcsv(beh, "00_data/zcsync_behavior.csv")
# wcsv(loc, "00_data/zcsync_foiegras.csv")

# save a copy of endtimes
# write.table(endtimesdf, file = "00_data/endtimes.csv", sep = ',', row.names = FALSE)

### output tag failure days
tagfailures <- endtimesdf[which(endtimesdf$faildate < endtimesdf$cee_clip), c('DeployID', 'faildate')]
# wcsv(tagfailures, "00_data/zcsync_tagfailuredates.csv")
