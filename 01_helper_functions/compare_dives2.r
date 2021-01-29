###
# compare_dives2.r
# calculate various metrics between two behavior records

compare_dives2 <- function(b1, b2, tz = "UTC") {
  # expects Start and End to be numeric seconds
  
  # take the midpoint of duration and depth
  b1[, 'DurationMean'] <- rowMeans(b1[, c('DurationMax', 'DurationMin')])
  b2[, 'DurationMean'] <- rowMeans(b2[, c('DurationMax', 'DurationMin')])

  b1[, 'DepthMean'] <- rowMeans(b1[, c('DepthMax', 'DepthMin')])
  b2[, 'DepthMean'] <- rowMeans(b2[, c('DepthMax', 'DepthMin')])
  
  # find the gaps
  gaps1 <- findgaps2(b1)
  gaps2 <- findgaps2(b2)
  
  # add msg id and remove messages
  b1[, 'msgid'] <- cumsum(b1$What == "Message")
  b2[, 'msgid'] <- cumsum(b2$What == "Message")
  b1 <- b1[b1$What != "Message", ]
  b2 <- b2[b2$What != "Message", ]
  
  # remove gaps
  if(gaps1$ngaps > 0) {
    for(i in 1:gaps1$ngaps) {
      b2$End[b2$End > gaps1$gap_st[i] & b2$End < gaps1$gap_en[i]] <- NA
    }
    b2 <- b2[!is.na(b2$End), ]
  }
  
  if(gaps2$ngaps > 0) {
    for(i in 1:gaps2$ngaps) {
      b1$End[b1$End > gaps2$gap_st[i] & b1$End < gaps2$gap_en[i]] <- NA
    }
    b1 <- b1[!is.na(b1$End), ]
  }
  
  # clip both records only to the time that they were both transmitting
  # times should be clipped to the one that starts the latest minus the time
  # difference between the latest start and the next nearest point in time
  # on the other tag
  
  start_prime <- max(c(min(b1$Start), min(b2$Start)))
  shorter_tag <- which(c(min(b1$Start), min(b2$Start)) == start_prime)[1]
  
  if(shorter_tag == 1) y2 <- b2
  if(shorter_tag == 2) y2 <- b1
  
  nearest_time_index <- matchtimes(start_prime, y2$Start[y2$Start >= start_prime])
  nearest_time <- y2$Start[y2$Start >=  start_prime][nearest_time_index]
  nearest_time_diff <- nearest_time - start_prime
  
  start <- start_prime - nearest_time_diff
  
  # do the same thing for the end
  end_prime   <- min(c(max(b1$End), max(b2$End)))
  shorter_tag <- which(c(max(b1$End), max(b2$End)) == end_prime)
  
  if(shorter_tag == 1) y2 <- b2
  if(shorter_tag == 2) y2 <- b1
  
  nearest_time_index <- matchtimes(end_prime, y2$End[y2$End <= end_prime])
  nearest_time <- y2$End[y2$End <= end_prime][nearest_time_index]
  nearest_time_diff <- end_prime - nearest_time
  
  end <- end_prime + nearest_time_diff
  
  # clip them out
  b1 <- b1[which(b1$Start >= start & b1$End <= end), ]
  b2 <- b2[which(b2$Start >= start & b2$End <= end), ]
  
  # assign overlap value
  overlap <- end - start
  
 # if the first row is a dive, then we need to add the start time
  if(b1$What[1] == "Dive") {
    newrow <- as.data.frame(b1[1, ])
    newrow[1, 1:ncol(newrow)] <- NA
    newrow[1, 'DeployID'] <- b1$DeployID[1]
    newrow[1, 'What'] <- "Surface"
    newrow[1, 'End'] <- b1$Start[1]
    b1 <- rbind(newrow, b1)
  }
  if(b2$What[1] == "Dive") {
    newrow <- as.data.frame(b2[1, ])
    newrow[1, 1:ncol(newrow)] <- NA
    newrow[1, 'DeployID'] <- b2$DeployID[1]
    newrow[1, 'What'] <- "Surface"
    newrow[1, 'End'] <- b2$Start[1]
    b2 <- rbind(newrow, b2)
  }

  # give everyone original row numbers for easy matching
  b1[, 'rownum'] <- 1:nrow(b1)
  b2[, 'rownum'] <- 1:nrow(b2)
  
  b1_dive_st <- b1[b1$What == "Surface", ]
  b1_dive_en <- b1[b1$What == "Dive", ]
  
  b2_dive_st <- b2[b2$What == "Surface", ]
  b2_dive_en <- b2[b2$What == "Dive", ]
  
  # match times for dives and surfaces (ends and beginnings of dives)
  match_st <- matchtimes(b1_dive_st$End, b2_dive_st$End)
  match_en <- matchtimes(b1_dive_en$End, b2_dive_en$End)
  
  match <- 1:nrow(b1)*NA
  match[b1_dive_st$rownum] <- b2_dive_st$rownum[match_st]
  match[b1_dive_en$rownum] <- b2_dive_en$rownum[match_en]
  
  t1 <- b1$End
  t2 <- b2$End[match]
  
  diff_times <- t2 - t1
  diff_durs <- b1$DurationMean - b2$DurationMean[match]
  diff_deps <- b1$DepthMean - b2$DepthMean[match]
  
  what1 <- sub("Surface", "dive_st", as.character(b1$What))
  what1 <- sub("Dive", "dive_en", what1)
  what2 <- sub("Surface", "dive_st", as.character(b2$What[match]))
  what2 <- sub("Dive", "dive_en", what2)
  
  deperror1 <- b1$DepthError
  deperror2 <- b2$DepthError[match]
  
  dur1 <- b1$DurationMean
  dur2 <- b2$DurationMean[match]
  
  dep1 <- b1$DepthMean
  dep2 <- b2$DepthMean
  
  # output some metrics
  list(
    tag1 = as.character(b1$DeployID[1]), 
    tag2 = as.character(b2$DeployID[1]),
    overlap = overlap,
    t1 = t1, 
    t2_matched = t2, 
    matchindices = match, 
    diff_times = diff_times, 
    diff_durs = diff_durs, 
    diff_deps = diff_deps, 
    what1 = what1, 
    what2 = what2, 
    msgid1 = b1$msgid, 
    msgid2 = b2$msgid, 
    deperror1 = deperror1, 
    deperror2 = deperror2, 
    dur1 = dur1, 
    dur2 = dur2, 
    dep1 = dep1, 
    dep2 = dep2
  )
}
