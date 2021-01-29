###
# buil_null_diver_shuffle.r
# make a null ziphius by resampling dives

build_null_diver2 <- function(b, deployid = "nulldiver01", replace = TRUE) {
  # get the grand start time
  st0 <- min(b$Start)
  
  # first split into dives and surfs
  surf <- b[b$What == "Surface", ]
  dive <- b[b$What == "Dive", ]
  
  # shuffle (default is with replacement)
  s0 <- surf[sample(1:nrow(surf), nrow(surf), replace = replace), ]
  d0 <- dive[sample(1:nrow(dive), nrow(dive), replace = replace), ]
  
  # intersperse (the times are still wrong and need to be corrected)
  b0 <- rbind(s0, d0)[order(c(seq_along(s0[, 1]), seq_along(d0[, 1]))), ]
  
  # calculate new times using average duration and the st0
  dur <- apply(b0[, c("DurationMin", "DurationMax")], 1, mean)
  b0$Start[1] <- st0
  b0$Start[2:nrow(b0)] <- st0 + cumsum(dur)[-length(dur)]
  b0$End[1:nrow(b0)] <- b0$Start + dur
  
  # make a message row
  msg <- b0[1, ]
  msg[, 6:ncol(msg)] <- NA
  msg$Start <- st0
  msg$End <- max(b0$End)
  msg$DurationMin <- msg$End - msg$Start
  msg$DurationMax <- msg$DurationMin
  msg$What <- "Message"
  
  # bind together and return
  b0 <- rbind(msg, b0)
  b0$DeployID <- deployid
  b0
}
