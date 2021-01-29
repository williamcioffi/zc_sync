###
# synsim_figure.r
# make a graph of synsim results

# load the data
load("../../00_data/zcsync_synsim_results.rdata")

# make some labels
out <- synsim_out
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
	
}

# calculate sim and obs
for(i in 1:length(out)) {
  out[[i]]$sim <- do.call('c',  out[[i]]$n_less_thresh_sim) / do.call('c',  out[[i]]$totaln_sim)
  out[[i]]$obs <- out[[i]]$n_less_thresh_obs / out[[i]]$totaln_obs
}



###
### GO MAKE FIGURE
###

# open graphics
pdf("synsim_all_MS.pdf", width = 5, height = 5)

par(mfrow = c(5, 5), mar = rep(0, 4), oma = c(5.1, 5.1, 0, 0))


# only those that overlap and were therefore modeled
desetags <- which(!sapply(out, is.null))[c(1, 2, 5, 21, 9, 18, 3:4, 6:8, 10:17, 19:20)]

# calculate ymax
ymax <- 0
for(i in desetags) {
  maxct <- max(hist(out[[i]]$sim, plot = FALSE, nclass = 10)$counts)
  ymax <- max(ymax, maxct)
}

count <- 1
for(i in desetags) {
  
	if(count == 21) {
		# plot(0, 0, type = 'n', xlab = "", ylab = "", axes = FALSE)
		# plot(0, 0, type = 'n', xlab = "", ylab = "", axes = FALSE)
		
		curout <- out[[i]]
		# plot(density(curout$sim), ylim = c(0, 100), xlim = c(0, 1), las = 1, xlab = "", ylab = "", main = "", axes = FALSE)
		hist(curout$sim, xlab = "", ylab = "", main = "", las = 1, xlim = c(-0.1, 1), ylim = c(0, ymax), yaxt = 'n', xaxt = 'n', col = "grey25", border = NA, nclass = 10)
		abline(v = curout$obs, lty = 2, col = "purple")
		box()
	} else {
		curout <- out[[i]]
		# plot(density(curout$sim), ylim = c(0, 100), xlim = c(0, 1), las = 1, xlab = "", ylab = "", main = "", axes = FALSE)
		hist(curout$sim, xlab = "", ylab = "", main = "", las = 1, xlim = c(-0.1, 1), ylim = c(0, ymax), yaxt = 'n', xaxt = 'n', col = "grey25", border = NA, nclass = 10)
		abline(v = curout$obs, lty = 2, col = "purple")	
		box()
	}
	
  # axes
	if(count == 21) {
	  axis(1, at = c(0, 0.25, 0.5, 0.75, 1.0))
	  axis(1, at = c(0, 0.25, 0.5, 0.75, 1.0), lab = NA, tcl = .3)
	  
	  axis(2, las = 1)
	  axis(2, lab = NA, las = 1, tcl = .3)
	} 
  
  if(count %in% c(17, 18, 19, 20)) {
    axis(1, at = c(0, 0.25, 0.5, 0.75, 1.0), lab = NA)
    axis(1, at = c(0, 0.25, 0.5, 0.75, 1.0), lab = NA, tcl = .3)
  }
  
  if(count %in% c(1, 6, 11, 16)) {
    axis(2, las = 1, lab = NA)
    axis(2, lab = NA, las = 1, tcl = .3)
  }
  
	grp <- "diff grp"
	if(curout$samegrp) grp <- "same grp"
	
	sex12 <- paste(curout$sex1, curout$sex2, sep = "-")
	id12 <- paste(curout$tag1, curout$tag2, sep = "-")
	id12 <- sub("ZcTag0", "", sub("ZcTag0", "", id12))

	# test stat
	test <- c(curout$sim, curout$obs)
	
	pval <-length(which(curout$obs <= test))/length(test)
	
  sig <- ""
	if(pval < 0.05) sig <- "*"
	
	legend("center", legend = c(id12, grp, sex12, paste0("p = ", pval, sig)), bty = 'n', cex = .75)
	legend("topleft", legend = paste0("(", tolower(LETTERS[count]), ")"), bty = 'n', cex = .75)
# text(.5, 400, count, cex = 10)
	
	count <- count + 1
}

mtext("porportion of events within 60 seconds", side = 1, line = 3.6, at = 3)
mtext("frequency", side = 2, line = 3.6, at = 1800)

# close graphics
dev.off()
