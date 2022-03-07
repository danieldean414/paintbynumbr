# key difference: getting rid of `library()` call
  # instead, every time you use a function, use <pkg_name>::<function>()

english <- temp <- NULL


lookup_table <- dplyr::tribble(
  ~where, ~english,
  "beach",     "US",
  "coast",     "US",
  "seashore",     "UK",
  "seaside",     "UK"
)

#' @export
localize_beach <- function(dat) {
  dplyr::left_join(dat, lookup_table)
}

f_to_c <- function(x) (x - 32) * 5/9

#' @export
celsify_temp <- function(dat) {
  dplyr::mutate(dat, temp = dplyr::if_else(english == "US", f_to_c(temp), temp))
}

now <- Sys.time()
#timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")

# use withr::local_*() functions to keep the changes local to timestamp()
timestamp <- function(time = Sys.time()) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  format(time, "%Y-%B-%d_%H-%M-%S")
}


#' @export
#outfile_path <- function(infile) {
#  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
#}

# allow user to provide a time, but default to "now"
outfile_path <- function(infile, time = Sys.time()) {
  ts <- timestamp(time)
  paste0(ts, "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}
