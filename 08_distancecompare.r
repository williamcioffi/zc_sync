###
# 08_distancecompare.r
# compare pairwise distance
#

# source helper functions
source("01_helper_functions/tagutils_helperfunctions.r")
source("01_helper_functions/matchtimes.r") # put in helper functions?
source("01_helper_functions/distance.r")

# constants
MIN_OVERLAP_DAYS <- 1 # minimum required overlapping days of data

# load data
load("00_data/zcsync_identify_synchrony.rdata")

loc <- rcsv("00_data/zcsync_foiegras.csv")
locl <- split(loc, loc$id)

# calculate pairwise distances
dl <- list()
for(i in 1:ntagpairs) {
  n1 <- tagpairs[[i]][[1]]
  n2 <- tagpairs[[i]][[2]]

  # grab the modeled positions
  z1 <- locl[[n1]]
  z2 <- locl[[n2]]
  
  # do an inner join
  z3 <- merge(z1, z2, by = 'date')

  # save the resulting dates
  datetime <- z3$date
  
  # calculate haversine distances
  dists <- latlond(z3$lat.x, z3$lon.x, z3$lat.y, z3$lon.y) # km

  if(!all(is.na(dists))) {
    out <- list(tag1 = n1, tag2 = n2, datetime = datetime, dists = dists)
  } else {
    out <- NULL
    warning(paste(i, ":", n1, n2, "don't overlap!"))
  }

dl[[i]] <- out
}

# make a median distance matrix
distmatrix <- matrix(data = NA, ntags, ntags)
ndistmatrix <- matrix(data = NA, ntags, ntags)
dimnames(distmatrix) <- list(names(locl), names(locl))
dimnames(ndistmatrix) <- list(names(locl), names(locl))

for(i in 1:ntagpairs) {
  if(!is.null(dl[[i]])) {
    dl_cur <- dl[[i]]
    n1 <- as.character(dl_cur$tag1)
    n2 <- as.character(dl_cur$tag2)
    datetimes <- dl_cur$d1

    dists <- dl_cur$dists

    # make it symmetical
    distmatrix[n1, n2] <- median(dists, na.rm = TRUE)
    distmatrix[n2, n1] <- median(dists, na.rm = TRUE)

    ndistmatrix[n1, n2] <- length(which(!is.na(dists)))
    ndistmatrix[n2, n1] <- length(which(!is.na(dists)))
  }
}

# make a binary predictor matrix synchronous or not
syn_pred <- matrix(0, ntags, ntags)
syn_pred[syntimes_en - syntimes_st > 60*60*24] <- 1
diag(syn_pred) <- NA
dimnames(syn_pred) <- list(names(locl), names(locl))

# save data for making figures
save.image("00_data/zcsync_distdat.RData")



###
### mantel test
###

# can only include a limited set of tags due to missingness
# look at first cohort
desetags <- c("ZcTag054", "ZcTag055", "ZcTag056", "ZcTag057", "ZcTag058")
pred_nomiss1 <- syn_pred[desetags, desetags]
resp_nomiss1 <- distmatrix[desetags, desetags]

# mantel test and mrqap
library(vegan)
mantel(pred_nomiss1, resp_nomiss1)



###
### do an asymptotic wilcoxon-mann-whitney test
###

# this incoperates all the tags, but doesn't account for
# network effects.

# use the coin implementation to deal with ties
library(coin)

distmatrix2 <- distmatrix
syn_pred2 <- syn_pred
distmatrix2[upper.tri(distmatrix2)] <- NA
syn_pred2[upper.tri(syn_pred2)] <- NA

# make a df
distdf <- data.frame(
  median_dist = as.vector(distmatrix2),
  synch = as.vector(syn_pred2)
)

# remove the NAs
distdf <- distdf[complete.cases(distdf), ]

# run the test n = 23 dyads
coin::wilcox_test(median_dist ~ as.factor(synch), data = distdf)
