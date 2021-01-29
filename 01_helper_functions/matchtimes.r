###
# matchtimes.r
# a tiny helper function for matching times to the nearest
# (it rounds up...)

matchtimes <- function(t1, t2) {
# t1, t2 are numeric
  findInterval(t1, c(-Inf, head(t2, -1)) + c(0, diff(t2)/2))
}
