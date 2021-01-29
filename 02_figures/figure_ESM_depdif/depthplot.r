###
# depthplot.r
# make a beanplot figure comparing depths among dyads

load("../../00_data/zcsync_divecdepcompare.rdata") # for ddep

library(beanplot)

# subset the tags to overlapping dyads
ddep <- ddep[!sapply(ddep, is.null)]

# add in the meta data
taggrps <- c("A", "A", "A", "B", "C", "D", "D", "E", "F", "F", "H", "I", "I")
tagnames <- c("ZcTag054", "ZcTag055", "ZcTag056", "ZcTag057", "ZcTag058", "ZcTag060", "ZcTag061", "ZcTag062", "ZcTag063", "ZcTag064", "ZcTag066", "ZcTag067", "ZcTag068")
sexes <- c("AM", "AM", "AM", "Other", "Other", "AM", "Other", "Other", "AM", "Other", "AM", "AM", "AM")


# tagged in same group + synchronous
for(i in 1:length(ddep)) {
  m1 <- match(ddep[[i]]$tag1, tagnames)
  m2 <- match(ddep[[i]]$tag2, tagnames)
  
  tagged_in_same_grp <- taggrps[m1] == taggrps[m2]
  
  if((taggrps[m1] == "A" | taggrps[m1] == "I") & tagged_in_same_grp) {
    synch <- TRUE
  } else {
    synch <- FALSE
  }
  
  ddep[[i]]$samegrp <- tagged_in_same_grp
  ddep[[i]]$synch <- synch
  ddep[[i]]$sex1 <- sexes[m1]
  ddep[[i]]$sex2 <- sexes[m2]
}

# remove 67 due to pressure transducer failure
ddep <- ddep[sapply(ddep, '[[', 'tag1') != "ZcTag067" & sapply(ddep, '[[', 'tag2') != "ZcTag067"]

# make a blank for plotting purposes
ddep[[20]] <- ddep[[19]]
for(i in 1:length(ddep[[20]])) {
  curthing <- ddep[[20]][[i]]
  if(mode(curthing) == "character") {
    ddep[[20]][[i]] <- ""
  } else if(mode(curthing) == "logical") {
    ddep[[20]][[i]] <- FALSE
  } else {
    ddep[[20]][[i]] <- vector()
  }
}

# make up the order
amamt <- c(1:2, 5)
amot <- c(9, 18)
spacer <- 20
amamf <- c(11, 19)
amof <- c(3:4, 6:7, 10, 12)
oamf <- c(14, 16)
oof <- c(8, 13, 15, 17)

desetags <- c(amamt, amot, spacer, amamf, amof, oamf, oof)

cols <- list()
outline <- vector()
xlabs <- vector()
k <- 1
for(i in desetags) {
  n1 <- as.character(ddep[[i]]$tag1)
  n2 <- as.character(ddep[[i]]$tag2)
  n1lab <- sub("ZcTag0", "", n1)
  n2lab <- sub("ZcTag0", "", n2)
  xlabs[k] <- paste(n1lab, n2lab, sep = '-')
  if(ddep[[i]]$synch) {
    fill <- "grey"
    outline[k] <- "grey"
  } else {
    fill <- "white"
    outline[k] <- "black"
  }
  cols[[k]] <- c(fill, "black", "black", "black")
  k <- k + 1
}

yytmp <- lapply(ddep, '[[', 'dep_diff')
yy <- lapply(yytmp[desetags], abs)



###
### go
###

pdf("depplot.pdf", width = 10, height = 6)

par(mar = c(5.6, 5.1, 2.1, 0))
beanplot(yy, las = 1, col = cols, axes = FALSE, border = outline, ll = .01)
mtext("depth difference (metres)", side = 2, line = 4.1)
mtext("tag ID pairs", side = 1, line = 4.1)
axis(2, las = 1, tcl = -0.3)
axis(2, lab = NA, tcl = 0.3)
axis(1, at = 1:5, labels = xlabs[1:5], las = 2, tcl = -0.3)
axis(1, at = 1:5, lab = NA, tcl = 0.3)
axis(1, at = 7:20, labels = xlabs[7:20], las = 2, tcl = -0.3)
axis(1, at = 7:20, lab = NA, tcl = 0.3)

# same group
axis(3, at = c(0.75, 3.25), lab = NA, tcl = .5)
mtext("AM-AM", line = 0, side = 3, at = 2, cex = .75)

axis(3, at = c(3.75, 5.25), lab = NA, tcl = .5)
mtext("AM-Other", side = 3, line = 0, at = 4.5, cex = .75)

mtext("same group", side = 3, line = 1.1, at = 3.25, cex = .75)

abline(v = 6, lty = 2)

# diff group
axis(3, at = c(6.75, 8.25), lab = NA, tcl = .5)
mtext("AM-AM", side = 3, line = 0, at = 7.5, cex = .75)

axis(3, at = c(8.75, 14.25), lab = NA, tcl = .5)
mtext("AM-Other", side = 3, line = 0, at = 12, cex = .75)

axis(3, at = c(14.75, 16.25), lab = NA, tcl = .5)
mtext("Other-AM", side = 3, line = 0, at = 15.5, cex = .75)

axis(3, at = c(16.75, 20.25), lab = NA, tcl = .5)
mtext("Other-Other", side = 3, line = 0, at = 18.5, cex = .75)

mtext("different group", side = 3, line = 1.1, at = 12.5, cex = .75)

dev.off()
