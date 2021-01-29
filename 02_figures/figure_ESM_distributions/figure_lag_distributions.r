###
# figure_lag_distributions.r
# make plots of the event lags between pairs of tags
# and the distributions of the lags

# CONSTANTS
ONEMINUTE <- 60 # seconds

# import data
load("../../00_data/zcsync_identify_synchrony.RData")

# remove dyads that don't overlap (null)
compdives_nonull <- compdives[!sapply(compdives, is.null)]

# a list to hold the results
difftime_bins <- list()

# bin in 60 second intervals
for(i in 1:length(compdives_nonull)) {
  # grab the dyad
  cur <- compdives_nonull[[i]]
  
  # abs value of the time differences
  difftimes <- cur$diff_times
  difftimes <- abs(difftimes)

  # calculate breaks  
  maxbin <- ceiling((max(difftimes) / ONEMINUTE)) * ONEMINUTE
  brks <- seq(from = 0, to = maxbin, by = ONEMINUTE)

  # make a table
  histotable <- table(cut(difftimes, brks, labels = FALSE))
  
  # make a data.frame
  difftime_bins[[i]] <- data.frame(
    tag1 = cur$tag1,
    tag2 = cur$tag2,
    bins = as.numeric(names(histotable))*ONEMINUTE,
    cnts = as.vector(histotable)
  )
}


# log - lag plot

idlabs <- sapply(difftime_bins, function(x) {
  paste(
    sub('ZcTag0', '', x$tag1[1]),
    sub('ZcTag0', '', x$tag2[1]),
    sep = '-'
  )
})

grplabs <- c(
  'same grp',
  'same grp',
  'diff grp',
  'diff grp',
  'same grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'same grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'diff grp',
  'same grp',
  'diff grp',
  'diff grp',
  'same grp'
)

sexlabs <- c(
  'AM-AM',
  'AM-AM',
  'AM-Other',
  'AM-Other',
  'AM-AM',
  'AM-Other',
  'AM-Other',
  'Other-Other',
  'AM-Other',
  'AM-Other',
  'AM-AM',
  'AM-Other',
  'Other-Other',
  'Other-AM',
  'Other-Other',
  'Other-AM',
  'Other-Other',
  'AM-Other',
  'AM-AM',
  'AM-AM',
  'AM-AM'
)

tagorder <- c(1, 2, 5, 21, 9, 18, 3, 4, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20)


alldf <- do.call('rbind', difftime_bins)


pdf('loglag.pdf', height = 6, width = 6)
par(mfrow = c(7, 3), mar = rep(0, 4), oma = c(5.1, 5.1, 0, 0))

for(i in 1:length(difftime_bins)) {
  cur <- difftime_bins[[tagorder[i]]]
  
  plot(alldf$bins / 60, log10(alldf$cnts),
    type = 'n', 
    xlim = c(0, 20),
    xlab = '',
    ylab = '',
    axes = FALSE
  )
  
  doxlabels <- FALSE
  doylabels <- FALSE
  if(i == 10) doylabels = TRUE
  if(i == 10) mtext(expression(log[10](n[events])), side = 2, line = 3.1)
  if(i == 20) doxlabels = TRUE
  if(i == 20) mtext('time difference (minutes)', side = 1, line = 3.1)
  
  axis(1, labels = FALSE, tcl = 0.3)
  axis(2, labels = FALSE, tcl = 0.3)
  axis(1, labels = doxlabels, tcl = -0.3)
  axis(2, labels = doylabels, tcl = -0.3, las = 1)
  box()
# plot(cur$bins / 60, (cur$cnts), type = 'n', xlim = c(0, 20))

  # i <- 1

  lines(cur$bins / 60, log10(cur$cnts))
  points(cur$bins / 60, log10(cur$cnts), pch = 16)
  legend('topright', legend = c(idlabs[tagorder[i]], grplabs[tagorder[i]], sexlabs[tagorder[i]]), bty = 'n')
  # i <- i + 1
}
dev.off()


# make a density plot

pdf('lagdensity.pdf', height = 6, width = 6)
par(mfrow = c(7, 3), mar = rep(0, 4), oma = c(5.1, 5.1, 0, 0))

diff_times_list <- sapply(compdives_nonull, '[[', 'diff_times')
for(i in 1:length(diff_times_list)) {
  cur <- abs(diff_times_list[[tagorder[i]]])
  
  cur <- cur[cur <= 1200 ]
  plot(density(cur/60), xlim = c(-50/60, 1200/60), ylim = c(0, 1.4),
    xlab = '',
    main = '',
    ylab = '',
    axes = FALSE
  )
  
  doxlabels <- FALSE
  doylabels <- FALSE
  if(i == 10) doylabels = TRUE
  if(i == 10) mtext('probability density', side = 2, line = 3.1)
  if(i == 20) doxlabels = TRUE
  if(i == 20) mtext('time difference (minutes)', side = 1, line = 3.1)
  
  axis(1, labels = FALSE, tcl = 0.3)
  axis(2, labels = FALSE, tcl = 0.3)
  axis(1, labels = doxlabels, tcl = -0.3)
  axis(2, labels = doylabels, tcl = -0.3, las = 1)
  box()
  
  legend('topright', legend = c(idlabs[tagorder[i]], grplabs[tagorder[i]], sexlabs[tagorder[i]]), bty = 'n')
}
dev.off()
