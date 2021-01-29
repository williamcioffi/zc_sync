###
# distplot.r
# beanplots of distances among dyads

load("../../00_data/zcsync_distdat.rdata")

# libs
library(beanplot)

# subset the tags to overlapping dyads
dl <- dl[!sapply(dl, is.null)]

# add in the meta data
taggrps <- c("A", "A", "A", "B", "C", "D", "D", "E", "F", "F", "H", "I", "I")
tagnames <- sort(unique(c(sapply(dl, '[[', 'tag1'), sapply(dl, '[[', 'tag2'))))
sexes <- c("AM", "AM", "AM", "Other", "Other", "AM", "Other", "Other", "AM", "Other", "AM", "AM", "AM")


# tagged in same group + synchronous
for(i in 1:length(dl)) {
  m1 <- match(dl[[i]]$tag1, tagnames)
  m2 <- match(dl[[i]]$tag2, tagnames)
  
  tagged_in_same_grp <- taggrps[m1] == taggrps[m2]
  
  if((taggrps[m1] == "A" | taggrps[m1] == "I") & tagged_in_same_grp) {
    synch <- TRUE
  } else {
    synch <- FALSE
  }
  
  dl[[i]]$samegrp <- tagged_in_same_grp
  dl[[i]]$synch <- synch
  dl[[i]]$sex1 <- sexes[m1]
  dl[[i]]$sex2 <- sexes[m2]
}

# make a blank for plotting purposes
dl[[24]] <- dl[[23]]
for(i in 1:length(dl[[24]])) {
  curthing <- dl[[24]][[i]]
  if(mode(curthing) == "character") {
    dl[[24]][[i]] <- ""
  } else if(mode(curthing) == "logical") {
    dl[[24]][[i]] <- FALSE
  } else {
    dl[[24]][[i]] <- vector()
  }
}

# get just the dists out
dists <- lapply(dl, '[[', 'dists')

# make up the order
amamt <- c(1, 2, 5, 23)
amot <- c(11, 20)
spacer <- 24
amamf <- c(13, 21:22)
amof <- c(3:4, 6:9, 12, 14)
oamf <- c(16, 18)
oof <- c(10, 15, 17, 19)

desetags <- c(amamt, amot,spacer, amamf, amof, oamf, oof)

# make labels and colors
cols <- list()
outline <- vector()
xlabs <- vector()
k <- 1
for(i in desetags) {
	n1 <- as.character(dl[[i]]$tag1)
	n2 <- as.character(dl[[i]]$tag2)
	n1lab <- sub("ZcTag0", "", n1)
	n2lab <- sub("ZcTag0", "", n2)
	xlabs[k] <- paste(n1lab, n2lab, sep = '-')
	if(dl[[i]]$sync) {
		fill <- "grey"
		outline[k] <- "grey"
	} else {
		fill <- "white"
		outline[k] <- "black"
	}
	cols[[k]] <- c(fill, "black", "black", "black")
	k <- k + 1
}

### open graphics
pdf("distplot.pdf", width = 10, height = 6)

par(mar = c(5.6, 5.1, 2.1, 0))
beanplot(dists[desetags], las = 1, col = cols, axes = FALSE, border = outline, ll = .01)
mtext("pairwise distance (kilometers)", side = 2, line = 4.1)
mtext("tag ID pairs", side = 1, line = 4.1)
axis(2, las = 1, tcl = -0.3)
axis(2, lab = NA, tcl = 0.3)
axis(1, at = 1:6, labels = xlabs[1:6], las = 2, tcl = -0.3)
axis(1, at = 1:6, labels = NA, las = 2, tcl = 0.3)
axis(1, at = 8:24, labels = xlabs[8:24], las = 2, tcl = -0.3)
axis(1, at = 8:24, labels = NA, las = 2, tcl = 0.3)


# same group
axis(3, at = c(0.75, 4.25), lab = NA, tcl = .5)
mtext("AM-AM", line = 0, side = 3, at = 2.5, cex = .75)

axis(3, at = c(4.75, 6.25), lab = NA, tcl = .5)
mtext("AM-Other", side = 3, line = 0, at = 5.5, cex = .75)

mtext("same group", side = 3, line = 1.1, at = 4, cex = .75)

# divider
abline(v = 7, lty = 2)

# diff group
axis(3, at = c(7.75, 10.25), lab = NA, tcl = 0.5)
mtext("AM-AM", side = 3, line = 0, at = 9, cex = .75)

axis(3, at = c(10.75, 18.25), lab = NA, tcl = 0.5)
mtext("AM-Other", side = 3, line = 0, at = 14.5, cex = .75)

axis(3, at = c(18.75, 20.25), lab = NA, tcl = 0.5)
mtext("Other-AM", side = 3, line = 0, at = 19.5, cex = .75)

axis(3, at = c(20.75, 24.25), lab = NA, tcl = 0.5)
mtext("Other-Other", side = 3, line = 0, at = 22.5, cex = .75)

mtext("different group", side = 3, line = 1.1, at = 15.5, cex = .75)

dev.off()
