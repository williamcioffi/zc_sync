###
# 07_depthcompare.r
# compare dive depth among dyads

# libs
library(vegan) # for mantel test
library(coin)  # for wilcox_test

# load data
load("00_data/zcsync_identify_synchrony.rdata")
load("00_data/zcsync_synsim_results.rdata")

# source functions
source("01_helper_functions/tagutils_helperfunctions.r")

# extract data
ddep <- list()
ddep_fordf <- list()
for(i in 1:length(tagpairs)) {
  ddep[[i]] <- NULL
  if(tagpairs[[i]]$overlap >= 1) {
    n1 <- tagpairs[[i]]$tag1
    n2 <- tagpairs[[i]]$tag2

    ddep_tmp <- compdives[[i]]$diff_deps
    ddep[[i]] <- list(tag1 = n1, tag2 = n2, dep_diff = ddep_tmp)
    ddep_fordf[[i]] <- quantile(abs(ddep_tmp), c(0.5, 0.25, 0.75, 0.025, 0.0975), na.rm = TRUE)
  }
}

names(ddep) <- names(compdives)

# ouput results for making figure
save(ddep, file = "00_data/zcsync_divecdepcompare.rdata")



###
### set up matrices
###

tagnames <- c("ZcTag054", "ZcTag055", "ZcTag056", "ZcTag057", "ZcTag058", "ZcTag060", "ZcTag061", "ZcTag062", "ZcTag063", "ZcTag064", "ZcTag066", "ZcTag067", "ZcTag068")
syn_pred <- matrix(0, ntags, ntags)
syn_pred[syntimes_en - syntimes_st > 60*60*24] <- 1
diag(syn_pred) <- NA
dimnames(syn_pred) <- list(tagnames, tagnames)

depmatrix <- matrix(data = NA, ntags, ntags)
dimnames(depmatrix) <- list(tagnames, tagnames)

for(i in 1:ntagpairs) {
  if(!is.null(ddep[[i]])) {
  depcur <- ddep[[i]]
  n1 <- as.character(depcur$tag1)
  n2 <- as.character(depcur$tag2)

  depmatrix[n1, n2] <- median(abs(depcur$dep_diff), na.rm = TRUE)
  depmatrix[n2, n1] <- median(abs(depcur$dep_diff), na.rm = TRUE)
  }
}



###
### look at first cohort with mantel test
###

# can only include a limited set of tags due to missingness

desetags <- c("ZcTag054", "ZcTag056", "ZcTag057", "ZcTag058")
pred_nomiss1 <- syn_pred[desetags, desetags]
resp_nomiss1 <- depmatrix[desetags, desetags]

vegan::mantel(pred_nomiss1, resp_nomiss1)



###
### do an asymptotic wilcoxon-mann-whitney test
###

# this incoperates all the tags, but doesn't account for
# network effects.

# use the coin implementation to deal with ties
library(coin)

depmatrix2 <- depmatrix
syn_pred2 <- syn_pred
depmatrix2[upper.tri(depmatrix2)] <- NA
syn_pred2[upper.tri(syn_pred2)] <- NA

# remove 67 (pressure sensor failure)
depmatrix2['ZcTag067', ] <- NA
depmatrix2[, 'ZcTag067'] <- NA

# make a df
depdf <- data.frame(
  median_dep_diff = as.vector(depmatrix2),
  synch = as.vector(syn_pred2)
)

# remove the NAs
depdf <- depdf[complete.cases(depdf), ]

# run the test n = 19 dyads
coin::wilcox_test(median_dep_diff ~ as.factor(synch), data = depdf)
