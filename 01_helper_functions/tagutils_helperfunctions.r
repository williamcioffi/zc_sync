###
# tagutils_helperfunctions.r
# some misc helper functions i use frequently
# they now live in github.com/williamcioffi/sattagutils

# sane defaults for reading in csvs
rcsv <- function(..., header = TRUE, sep = ',', stringsAsFactors = FALSE) {
  read.table(..., header = header, sep = sep, stringsAsFactors = stringsAsFactors)
}

# sane dfeaults for writing out csvs
wcsv <- function(..., row.names = FALSE, sep = ',') {
  write.table(..., row.names = row.names, sep = sep)
}

# convert from posix to numerical
date2num <- function(d, tz = 'UTC', format = '%H:%M:%S %d-%b-%Y') {
  as.numeric(as.POSIXct(d, tz = tz, format = format))
}

# convert from numeric to posix
num2date <- function(d, tz = 'UTC', origin = "1970-01-01", outformat = NA) {
  out <- as.POSIXct(d, tz = tz , origin = origin)
  if(!is.na(outformat)) out <- format(out, outformat)
  
  out
}

# make a seq of days or hours from a datenum
dateseq <- function(d, hours = FALSE) {
  unit <- 60*60*24 # day in seconds
  if(hours) unit <- 60*60 # hour in seconds

  mind <- min(d, na.rm = TRUE)
  maxd <- max(d, na.rm = TRUE)

  std <- trunc(mind / unit) * unit
  end <- ceiling(maxd / unit) * unit
  
  seq(std, end, by = unit)
}
