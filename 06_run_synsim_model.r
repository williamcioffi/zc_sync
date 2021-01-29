###
# 06_run_synsim_model.r
# run synsim model

# constants
THRESHOLD_SECONDS <- 60 # the tag time accuracy

# libs
library(doParallel)
library(doRNG)

# source helper functions
source("01_helper_functions/tagutils_helperfunctions.r")
source("01_helper_functions/findgaps.r")
source("01_helper_functions/matchtimes.r")
source("01_helper_functions/compare_dives2.r")
source("01_helper_functions/build_null_diver_shuffle.r")

# load data
load("00_data/zcsync_identify_synchrony.rdata")
beh <- rcsv("00_data/zcsync_behavior.csv")
behl <- split(beh, beh$DeployID)

# # socket for monitoring
# LOG_SOCKET <- 5555
# 
# # monitor in real time
# skt <- make.socket(port = LOG_SOCKET)
# print(paste0("logging on socket: ", LOG_SOCKET))
# 
# log <- function(m, ...) {
#   logmsg <- sprintf(paste0(as.character(Sys.time()), ": ", m, "\n"), ...)
#   write.socket(skt, logmsg)
# }

# start the cluster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# load the seed
load("00_data/rngseed.rdata")
RNGseed(seed)



###
### go
###

timest <- Sys.time()
synsim_out <- foreach(i = 1:length(compdives)) %dorng% {
# log("here we go: %d", i)
  cd <- compdives[[i]]
  out <- NULL

  if(!is.null(cd)) {
    # get the difftimes for convenience
    dts <- cd$diff_times

    b1 <- behl[[cd$tag1]]
    b2 <- behl[[cd$tag2]]

    st <- max(min(b1$Start), min(b2$Start))
    en <- min(max(b1$End), max(b2$End))

    b1trim <- b1[b1$Start >= st & b1$End <= en, ]
    b2trim <- b2[b2$Start >= st & b2$End <= en, ]

    b1 <- b1trim
    b2 <- b2trim

    # calculate overlap
    total_overlapdur_obs <- cd$overlap 

    # lists to contain the metrics
    n_less_thresh_sim <- list()
    totaln_sim <- list()
    stretch_durs_sim <- list()
    stretch_durdiff_sim <- list()
    stretch_lengths_sim <- list()
    total_overlapdur_sim <- vector()

    # these are all one value each
    n_less_thresh_obs <- vector()
    totaln_obs <- vector()
    stretch_durs_obs <- vector()
    stretch_durdiff_obs <- vector()
    stretch_lengths_obs <- vector()

    nsim <- 999
    # nreport <- 10
    # pb <- txtProgressBar(style = 3)

    # run the simulation
    for(s in 1:nsim) {
      # if(s %% nreport == 0)  setTxtProgressBar(pb, s / nsim)
      n1 <- build_null_diver2(b1, deployid = "n1", replace = FALSE)
      n2 <- build_null_diver2(b2, deployid = "n2", replace = FALSE)

      comsim <- compare_dives2(n1, n2)

      # calculate some metrics
      totaln_sim[[s]] <- length(comsim$diff_times)

      # how many are within the threshold
      deseless <- abs(comsim$diff_times) <= THRESHOLD_SECONDS
      n_less_thresh_sim[[s]] <- length(which(deseless))

      # how long is the duration over which these calcs are made
      total_overlapdur_sim[s] <- comsim$overlap

      # what are the durations of sychronous behavior (number of events and seconds)
      deseless <- abs(comsim$diff_times) <= THRESHOLD_SECONDS
      stretchid <- c(1, cumsum(abs(diff(deseless))) + 1)

      u_stretchid <- unique(stretchid[deseless])

      ##
      ## calc duration of synchronous stretches for each sim
      ##

      durs <- vector()
      durdiff <- vector()
      stretchlengths <- vector()

      if(length(u_stretchid > 0)) {
        count <- 1
        for(p in 1:length(u_stretchid)) {
          curstretch <- which(stretchid == u_stretchid[p])
          if(length(curstretch) > 1) {
            dur1 <- comsim$t1[curstretch][length(curstretch)] - comsim$t1[curstretch][1]
            dur2 <- comsim$t2[curstretch][length(curstretch)] - comsim$t2[curstretch][1]

            durs[count] <- mean(dur1, dur2)
            durdiff[count] <- dur1 - dur2
            stretchlengths[count] <- length(curstretch)
            count <- count + 1
          }
        }
      }

      if(length(durs) == 0) durs <- NA
      if(length(durdiff) == 0) durdiff <- NA
      if(length(stretchlengths) == 0) stretchlengths <- NA

      stretch_durs_sim[[s]] <- durs
      stretch_durdiff_sim[[s]] <- durdiff
      stretch_lengths_sim[[s]] <- stretchlengths
    }

    # calculate some observed values
    n_less_thresh_obs <- length(which(abs(dts) <= THRESHOLD_SECONDS))
    totaln_obs <- length(dts)


    ##
    ## calculate duration of synchronous stretches in the obs data
    ##

    # what are the durations of sychronous behavior (number of events and seconds)
    deseless <- abs(cd$diff_times) <= THRESHOLD_SECONDS
    stretchid <- c(1, cumsum(abs(diff(deseless))) + 1)

    u_stretchid <- unique(stretchid[deseless])

    # calc duration of synchronous stretches
    durs <- vector()
    durdiff <- vector()
    stretchlengths <- vector()

    if(length(u_stretchid > 0)) {
      count <- 1
      for(p in 1:length(u_stretchid)) {
        curstretch <- which(stretchid == u_stretchid[p])
        if(length(curstretch) > 1) {
          dur1 <- cd$t1[curstretch][length(curstretch)] - cd$t1[curstretch][1]
          dur2 <- cd$t2[curstretch][length(curstretch)] - cd$t2[curstretch][1]

          durs[count] <- mean(dur1, dur2)
          durdiff[count] <- dur1 - dur2
          stretchlengths[count] <- length(curstretch)
          count <- count + 1
        }
      }
    }

    if(length(durs) == 0) durs <- NA
    if(length(durdiff) == 0) durdiff <- NA
    if(length(stretchlengths) == 0) stretchlengths <- NA

    stretch_durs_obs <- durs
    stretch_durdiff_obs <- durdiff
    stretch_lengths_obs <- stretchlengths

    # build up the output list
    out <- list(
      tag1 = cd$tag1,
      tag2 = cd$tag2,
      n_less_thresh_sim = n_less_thresh_sim,
      totaln_sim = totaln_sim,
      stretch_durs_sim = stretch_durs_sim,
      stretch_durdiff_sim = stretch_durdiff_sim,
      stretch_lengths_sim = stretch_lengths_sim,
      total_overlapdur_sim = total_overlapdur_sim,
      n_less_thresh_obs = n_less_thresh_obs,
      totaln_obs = totaln_obs,
      stretch_durs_obs = stretch_durs_obs,
      stretch_durdiff_obs = stretch_durdiff_obs,
      stretch_lengths_obs = stretch_lengths_obs,
      total_overlapdur_obs = total_overlapdur_obs
    )
  }
  # return
  out
}
Sys.time() - timest
stopCluster(cl)

# close the log socket
# close.socket(skt)

names(synsim_out) <- sapply(compdives, function(l) paste(l$tag1, l$tag2, sep = "-"))
save(synsim_out, file = "00_data/zcsync_synsim_results.rdata")
