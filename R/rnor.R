##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param n
##' @param mean
##' @param sd
##' @param low_threshold
rnor <- function(n, mean, sd, low_threshold){
  pmax(rnorm(n = n, mean = mean, sd = sd), low_threshold)
}
