###
# figure_time.r
# make a timeline showing synchrony for dyads tagged in
# same group, different groups, and an example to show
# how the null model works

# source helper functions
source("../../01_helper_functions/tagutils_helperfunctions.r")
source("../../01_helper_functions/findgaps.r")
source("../../01_helper_functions/compare_dives2.r")
source("../../01_helper_functions/build_null_diver_shuffle.r")
source("../../01_helper_functions/plot_dives.r")
source("../../01_helper_functions/matchtimes.r")

# load data
load("../../00_data/zcsync_identify_synchrony.rdata")
beh <- rcsv("../../00_data/zcsync_behavior.csv")
behl <- split(beh, beh$DeployID)

# tiny helper function
getsten <- function(b1, b2) {
  # clip time clips both records only to the time that they were both transmitting
  # times should be clipped to the one that starts the latest minus the time
  # difference between the latest start and the next nearest point in time
  # on the other tag
  
  b1 <- b1[b1$What != "Message", ]
  b2 <- b2[b2$What != "Message", ]
  
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
  
  # return
  list(st = start, en = end)
}


# prepare compdives
out <- compdives
dese <- sapply(out, function(s) length(s$tag1) > 0)
out <- out[dese]

taggrps <- c("A", "A", "A", "B", "C", "D", "D", "E", "F", "F", "H", "I", "I")
tagnames <- sort(unique(c(sapply(out, '[[', 'tag1'), sapply(out, '[[', 'tag2'))))
sexes <- c("AM", "AM", "AM", "Other", "Other", "AM", "Other", "Other", "AM", "Other", "AM", "AM", "AM")

# tagged in same group + synchronous
for(i in 1:length(out)) {
  m1 <- match(out[[i]]$tag1, tagnames)
  m2 <- match(out[[i]]$tag2, tagnames)
  
  tagged_in_same_grp <- taggrps[m1] == taggrps[m2]
  
  if((taggrps[m1] == "A" | taggrps[m1] == "I") & tagged_in_same_grp) {
    synch <- TRUE
  } else {
    synch <- FALSE
  }
  
  out[[i]]$samegrp <- tagged_in_same_grp
  out[[i]]$synch <- synch
  out[[i]]$sex1 <- sexes[m1]
  out[[i]]$sex2 <- sexes[m2]
  out[[i]]$grp1 <- taggrps[m1]
  out[[i]]$grp2 <- taggrps[m2]
  
}
compdives <- out




###
### plot timelines
###

# calculate lims
samegrouptags <- c(1, 2, 5, 21, 9, 18)
allend <- 0
for(i in samegrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  relmax <- max(b1$End, b2$End)
  if(relmax > allend) allend <- relmax
}


# timeline zoom just look at the first four days of the synchs
FOURDAYS <- 60*60*24*4 # seconds
pdf("figure_ESM_timeline_zoom.pdf", height = 9, width = 15)
par(mfrow = c(6, 1), mar = c(0, 3.1, 0, 1.1), oma = c(6.1, 0, 0, 0), xpd = FALSE)
for(i in samegrouptags) {
  
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = TRUE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = FOURDAYS, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()

  # plot any data gaps
  g1 <- findgaps2(b1)
  g2 <- findgaps2(b2)
  
  if(g1$ngaps > 0) {
    rect(g1$gap_st, 0+maxy*0.1, g1$gap_en, maxy*2, col = "grey75", border = NA)
  }
  if(g2$ngaps > 0) {
    rect(g2$gap_st, 0+maxy*0.1, g2$gap_en, maxy*2, col = "grey75", border = NA)
  }
  
  
  dts <- abs(cd$diff_times)
  
  xx <- cd$t1[dts <= 60] - absmin
  yy <- rep(maxy/2, length(xx))
  points(xx[xx < FOURDAYS], yy[xx < FOURDAYS], pch = 16, cex = 2)
  
  xx <- cd$t1[dts > 60] - absmin
  yy <- rep(maxy/1.1, length(xx))
  points(xx[xx < FOURDAYS], yy[xx < FOURDAYS], cex = 2)
  
  # plot daily vert bars
  ds <- dateseq(c(0, FOURDAYS))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  lab1 <- sub("ZcTag0", "", cd$tag1)
  lab2 <- sub("ZcTag0", "", cd$tag2)
  
  mtext(
    paste0(
      lab1, "-", lab2, "\n",
      cd$sex1, "-", cd$sex2, "\n",
      "Grp ", cd$grp1
    ),
    side = 2, line = -3.1, las = 1
  )
}

axis(1, at = ds, lab = 0:(length(ds)-1), lty = 0, tick = FALSE, padj = 0.5, cex.axis = 1.5)
mtext("days since deployment", side = 1, line = 4.1, cex = 1.5)
dev.off()


# time line synch

pdf("figure_ms_timeline.pdf", height = 9, width = 15)
par(mfrow = c(6, 1), mar = c(0, 3.1, 0, 1.1), oma = c(6.1, 0, 0, 0), xpd = FALSE)
for(i in samegrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = TRUE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = allend, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()

  # plot any data gaps
  g1 <- findgaps2(b1)
  g2 <- findgaps2(b2)
  
  if(g1$ngaps > 0) {
    rect(g1$gap_st, 0+maxy*0.1, g1$gap_en, maxy*2, col = "grey75", border = NA)
  }
  if(g2$ngaps > 0) {
    rect(g2$gap_st, 0+maxy*0.1, g2$gap_en, maxy*2, col = "grey75", border = NA)
  }
  
  
  dts <- abs(cd$diff_times)
  points(cd$t1[dts <= 60] - absmin, rep(maxy/2, length(which(dts <= 60))), pch = 16, cex = 2)
  points(cd$t1[dts > 60] - absmin,  rep(maxy/1.1, length(which(dts > 60))), cex = 2)
  
  
  # plot daily vert bars
  ds <- dateseq(c(0, allend))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  lab1 <- sub("ZcTag0", "", cd$tag1)
  lab2 <- sub("ZcTag0", "", cd$tag2)
  
  mtext(
    paste0(
      lab1, "-", lab2, "\n",
      cd$sex1, "-", cd$sex2, "\n",
      "Grp ", cd$grp1
    ),
    side = 2, line = -3.1, las = 1
  )
}

axis(1, at = ds, lab = 0:(length(ds)-1), lty = 0, tick = FALSE, padj = 0.5, cex.axis = 1.5)
mtext("days since deployment", side = 1, line = 4.1, cex = 1.5)

dev.off()



###
### different group timelines
###

### do the long ones first
diffgrouptags <- c(6, 7, 8)
allend1 <- 0
for(i in diffgrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  relmax <- max(b1$End, b2$End)
  if(relmax > allend1) allend1 <- relmax
}

# plot timelines
pdf("figure_ESM_diffgrp1_timeline.pdf", height = 6, width = 20)
par(mfrow = c(3, 1), mar = c(0, 5.1, 0, 1.1), oma = c(6.1, 0, 0, 0), xpd = FALSE)
for(i in diffgrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = TRUE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = allend1, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()
  
  # plot any data gaps
  g1 <- findgaps2(b1)
  g2 <- findgaps2(b2)
  
  if(g1$ngaps > 0) {
    rect(g1$gap_st, 0+maxy*0.1, g1$gap_en, maxy*2, col = "grey75", border = NA)
  }
  if(g2$ngaps > 0) {
    rect(g2$gap_st, 0+maxy*0.1, g2$gap_en, maxy*2, col = "grey75", border = NA)
  }
  
  
  dts <- abs(cd$diff_times)
  points(cd$t1[dts <= 60] - absmin, rep(maxy/2, length(which(dts <= 60))), pch = 16, cex = 2)
  points(cd$t1[dts > 60] - absmin,  rep(maxy/1.1, length(which(dts > 60))), cex = 2)
  
  
  # plot daily vert bars
  ds <- dateseq(c(0, allend1))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  lab1 <- sub("ZcTag0", "", cd$tag1)
  lab2 <- sub("ZcTag0", "", cd$tag2)
  
  mtext(
    paste0(
      lab1, "-", lab2, "\n",
      cd$sex1, "-", cd$sex2, "\n",
      "Grps ", cd$grp1, "-", cd$grp2
    ),
    side = 2, line = -3.1, las = 1
  )
}

axis(1, at = ds, lab = 0:(length(ds)-1), lty = 0, tick = FALSE, padj = 0.5, cex.axis = 1.5)
mtext("days since deployment", side = 1, line = 4.1, cex = 1.5)

dev.off()



###
### do the smalls pt 1
###

diffgrouptags <- c(3:4, 10, 13, 19:20)
allend2 <- 0
for(i in diffgrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  relmax <- max(b1$End, b2$End)
  if(relmax > allend2) allend2 <- relmax
}

# plot timelines
pdf("figure_ESM_diffgrp2_timeline.pdf", height = 12, width = 20)
par(mfrow = c(6, 1), mar = c(0, 5.1, 0, 1.1), oma = c(6.1, 0, 0, 0), xpd = FALSE)
for(i in diffgrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = TRUE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = allend2, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()
  
  # plot any data gaps
  g1 <- findgaps2(b1)
  g2 <- findgaps2(b2)
  
  if(g1$ngaps > 0) {
    rect(g1$gap_st, 0+maxy*0.1, g1$gap_en, maxy*2, col = "grey75", border = NA)
  }
  if(g2$ngaps > 0) {
    rect(g2$gap_st, 0+maxy*0.1, g2$gap_en, maxy*2, col = "grey75", border = NA)
  }
  
  
  dts <- abs(cd$diff_times)
  points(cd$t1[dts <= 60] - absmin, rep(maxy/2, length(which(dts <= 60))), pch = 16, cex = 2)
  points(cd$t1[dts > 60] - absmin,  rep(maxy/1.1, length(which(dts > 60))), cex = 2)
  
  
  # plot daily vert bars
  ds <- dateseq(c(0, allend2))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  lab1 <- sub("ZcTag0", "", cd$tag1)
  lab2 <- sub("ZcTag0", "", cd$tag2)
  
  mtext(
    paste0(
      lab1, "-", lab2, "\n",
      cd$sex1, "-", cd$sex2, "\n",
      "Grps ", cd$grp1, "-", cd$grp2
    ),
    side = 2, line = -3.1, las = 1
  )
}

axis(1, at = ds, lab = 0:(length(ds)-1), lty = 0, tick = FALSE, padj = 0.5, cex.axis = 1.5)
mtext("days since deployment", side = 1, line = 4.1, cex = 1.5)

dev.off()



###
### do the smalls2
###

diffgrouptags <- c(11:12, 14:17)
allend2 <- 0
for(i in diffgrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  relmax <- max(b1$End, b2$End)
  if(relmax > allend2) allend2 <- relmax
}

# plot timelines
pdf("figure_ESM_diffgrp3_timeline.pdf", height = 12, width = 20)
par(mfrow = c(6, 1), mar = c(0, 5.1, 0, 1.1), oma = c(6.1, 0, 0, 0), xpd = FALSE)
for(i in diffgrouptags) {
  cd <- compdives[[i]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = TRUE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = allend2, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()
  
  # plot any data gaps
  g1 <- findgaps2(b1)
  g2 <- findgaps2(b2)
  
  if(g1$ngaps > 0) {
    rect(g1$gap_st, 0+maxy*0.1, g1$gap_en, maxy*2, col = "grey75", border = NA)
  }
  if(g2$ngaps > 0) {
    rect(g2$gap_st, 0+maxy*0.1, g2$gap_en, maxy*2, col = "grey75", border = NA)
  }
  
  
  dts <- abs(cd$diff_times)
  points(cd$t1[dts <= 60] - absmin, rep(maxy/2, length(which(dts <= 60))), pch = 16, cex = 2)
  points(cd$t1[dts > 60] - absmin,  rep(maxy/1.1, length(which(dts > 60))), cex = 2)
  
  
  # plot daily vert bars
  ds <- dateseq(c(0, allend2))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  lab1 <- sub("ZcTag0", "", cd$tag1)
  lab2 <- sub("ZcTag0", "", cd$tag2)
  
  mtext(
    paste0(
      lab1, "-", lab2, "\n",
      cd$sex1, "-", cd$sex2, "\n",
      "Grps ", cd$grp1, "-", cd$grp2
    ),
    side = 2, line = -3.1, las = 1
  )
}

axis(1, at = ds, lab = 0:(length(ds)-1), lty = 0, tick = FALSE, padj = 0.5, cex.axis = 1.5)
mtext("days since deployment", side = 1, line = 4.1, cex = 1.5)

dev.off()



###
### sim timeline example
###

# set a seed for the sims
# my orcid is 0000-0003-1182-8578
set.seed(311828578)

# output pdf
pdf("figure_ESM_nulltimeline.pdf", height = 12, width = 20)
par(mfrow = c(6, 1), mar = c(0, 3.1, 0, 1.1), oma = c(6.1, 0, 0, 0), xpd = FALSE)

# do it once for real
  cd <- compdives[[1]]
  b1 <- behl[[cd$tag1]]
  b2 <- behl[[cd$tag2]]
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  # save a copy for later
  b1.orig <- b1
  b2.orig <- b2
  
  relend <- en - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = TRUE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = relend, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()
  
  # plot any data gaps
  g1 <- findgaps2(b1)
  g2 <- findgaps2(b2)
  
  if(g1$ngaps > 0) {
    rect(g1$gap_st, 0+maxy*0.1, g1$gap_en, maxy*2, col = "grey75", border = NA)
  }
  if(g2$ngaps > 0) {
    rect(g2$gap_st, 0+maxy*0.1, g2$gap_en, maxy*2, col = "grey75", border = NA)
  }
  
  
  dts <- abs(cd$diff_times)
  points(cd$t1[dts <= 60] - absmin, rep(maxy/2, length(which(dts <= 60))), pch = 16, cex = 2)
  points(cd$t1[dts > 60] - absmin,  rep(maxy/1.1, length(which(dts > 60))), cex = 2)
  
  
  # plot daily vert bars
  ds <- dateseq(c(0, allend))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  lab1 <- sub("ZcTag0", "", cd$tag1)
  lab2 <- sub("ZcTag0", "", cd$tag2)
  
  mtext(
    paste0(
      lab1, "-", lab2, "\n",
      cd$sex1, "-", cd$sex2, "\n",
      "Grp ", cd$grp1, "\n",
      "\nobserved"
    ),
    side = 2, line = -3.1, las = 1
  )
# 
# # blank
# plot(0, 0, type = 'n', xlab = "", ylab = "", axes = FALSE)
# abline(h = 0, lwd = 2)

# do five sims
for(i in 1:5) {
  n1 <- build_null_diver2(b1.orig, deployid = "n1", replace = FALSE)
  n2 <- build_null_diver2(b2.orig, deployid = "n2", replace = FALSE)
  cd <- compare_dives2(n1, n2)
  
  b1 <- n1
  b2 <- n2
  
  sten <- getsten(b1, b2)
  
  st <- sten$st
  en <- sten$en
  
  b1trim <- b1[b1$Start >= st & b1$End <= en, ]
  b2trim <- b2[b2$Start >= st & b2$End <= en, ]
  
  b1 <- b1trim
  b2 <- b2trim
  
  absmin <- min(b1$Start, b2$Start)
  
  b1$Start <- b1$Start - absmin
  b1$End <- b1$End - absmin
  b2$Start <- b2$Start - absmin
  b2$End <- b2$End - absmin
  
  # relend <- en - absmin
  
  miny <- -max(rbind(b1, b2)$DepthMax, na.rm = TRUE)
  maxy <- abs(miny)*.2
  
  plot_dives2(rbind(b1, b2), xaxt = FALSE, yaxt = FALSE, ylab = "", show_gaps = FALSE, ylim = c(miny, maxy), hidelegend = TRUE, end_time = relend, pch = NA, hide_gap_lab = TRUE, lwd = 2, col = c(rgb(230/255, 25/255, 75/255, .5), rgb(67/255, 99/255, 216/255, .5)))
  # box()
  
  dts <- abs(cd$diff_times)
  points(cd$t1[dts <= 60] - absmin, rep(maxy/2, length(which(dts <= 60))), pch = 16, cex = 2)
  points(cd$t1[dts > 60] - absmin,  rep(maxy/1.1, length(which(dts > 60))), cex = 2)
  
  
  # plot daily vert bars
  ds <- dateseq(c(0, allend))
  ds <- ds - ds[1]
  abline(v = ds, lty = 2)
  
  mtext(
    paste("sim", i),
    side = 2, line = -3.1, las = 1
  )
}

axis(1, at = ds, lab = 0:(length(ds)-1), lty = 0, tick = FALSE, padj = 0.5, cex.axis = 1.5)
mtext("days since deployment", side = 1, line = 4.1, cex = 1.5)

dev.off()
