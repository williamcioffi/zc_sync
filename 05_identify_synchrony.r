###
# 05_identify_synchrony.r
# identify synchrony and set upsome useful data structures

# source helper functions
source("01_helper_functions/tagutils_helperfunctions.r")
source("01_helper_functions/findgaps.r")
source("01_helper_functions/compare_dives2.r")

# constants
MIN_OVERLAP_DAYS <- 1 # min overlap for a dyad in days to be included
MAX_NONSYNC_EVENTS <- 4 # maximum number of conseq events can be nonsync

# load data
beh <- rcsv("00_data/zcsync_behavior.csv")
behl <- split(beh, beh$DeployID)

# compare all dyads and determine which overlap for behavior data
ntags <- length(behl)
ntagpairs <- ntags*(ntags-1) / 2
tagpairs_tmp <- t(combn(1:ntags, 2))

# new list of tag pairs
tagpairs <- list()
for(i in 1:ntagpairs) {
  n1 <- names(behl)[tagpairs_tmp[i, 1]]
  n2 <- names(behl)[tagpairs_tmp[i, 2]]

  z1 <- behl[[n1]]
  z2 <- behl[[n2]]

  maxst <- max(z1$Start[1], z2$Start[1])
  minen <- min(z1$End[nrow(z1)], z2$End[nrow(z2)])

  # this is precisely how much the data records overlap
  # for the amount of overlap considered for matching look at compare_dives2()
  # output
  overlap <- (minen - maxst) / 60 / 60 / 24 # in days
  if(overlap < 0) overlap <- 0

  tagpairs[[i]] <- list(tag1 = n1, tag2 = n2, overlap = overlap)
}

# identify start and end of synchrony
# this isn't used in the analysis but is convenient later on
syntimes_st <- matrix(NA, ntags, ntags)
syntimes_en <- matrix(NA, ntags, ntags)

dimnames(syntimes_st) <- list(names(behl), names(behl))
dimnames(syntimes_en) <- list(names(behl), names(behl))

# calculate synchronous overlap times
# agnostic to whether animals were tagged in the same group or not
compdives <- list()

for(i in 1:ntagpairs) {
  n1 <- tagpairs[[i]][[1]]
  n2 <- tagpairs[[i]][[2]]

  z1 <- behl[[n1]]
  z2 <- behl[[n2]]

  # only compare if there is more than one day of data
  if(tagpairs[[i]]$overlap > MIN_OVERLAP_DAYS) {
    com <- compare_dives2(z1, z2)
    absdiff <- abs(com$diff_times)

    if(any(absdiff <= 60)) {
      # dyad is synchronous until 4 consecutive events are more than 60 seconds 
      # apart.
      stretches <- c(1, cumsum(abs(diff(absdiff > 60))) + 1)
      end_stretch <- min(rle(stretches[absdiff > 60])$values[rle(stretches[absdiff > 60])$lengths >= 4])

      # if there aren't any this will return inf or -inf
      if(is.infinite(end_stretch)) {
        ensyn <- min(max(com$t1), max(com$t2_matched))
      } else {
        ensynindex <- min(which(stretches == end_stretch))
        ensyn <- min(com$t1[ensynindex], com$t2_matched[ensynindex])
      }

      # assign start of synchrony to the start of the record
      stsyn <- min(min(com$t1), min(com$t2_matched))

      # assign dates to matrix symetrically
      syntimes_st[n1, n2] <- stsyn
      syntimes_st[n2, n1] <- stsyn
      syntimes_en[n1, n2] <- ensyn
      syntimes_en[n2, n1] <- ensyn
    }
  } else {
    com <- NULL
    warning(paste(i, ":", n1, n2, "don't overlap!"))
  }
  
  compdives[[i]] <- com
}

# no synchronous time the start and end will be the same
# and so these are set to NA
dese <- syntimes_st == syntimes_en
syntimes_st[dese] <- NA
syntimes_en[dese] <- NA

# give compdives helpful names
names(compdives) <- sapply(compdives, function(l) {
  nam <- "no overlap"
  if(!is.null(l)) nam <- paste(l$tag1, l$tag2)
  nam
})

# save output
save(
  ntags,
  ntagpairs,
  tagpairs,
  syntimes_st,
  syntimes_en,
  compdives,
  file = "00_data/zcsync_identify_synchrony.rdata"
)
