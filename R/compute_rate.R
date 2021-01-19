##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @param exposure_rate
##' @param exposure_type
compute_rate <- function(x, exposure_rate, exposure_type) {

  stopifnot(exposure_rate %in% 0:2 & exposure_type %in% 1:4)
  
  switch(as.character(exposure_type),
         "1" = .compute_rate_1(x, exposure_rate),
         "2" = .compute_rate_2(x, exposure_rate),
         "3" = .compute_rate_3(x, exposure_rate),
         "4" = .compute_rate_4(x, exposure_rate))

}

.compute_rate_1 <- function(x, exposure_rate){
  
  log_intercept <- switch(as.character(exposure_rate),
                          "0" = 2,
                          "1" = 5,
                          "2" = 10)
  
  exp(log(log_intercept + 1) / 5 * x) - 1
  
}

.compute_rate_2 <- function(x, exposure_rate){
  
  coefficient <- switch(as.character(exposure_rate),
                        "0" = 2/5,
                        "1" = 1,
                        "2" = 2)
  
  coefficient * x
  
}

.compute_rate_3 <- function(x, exposure_rate){
  
  coefficient <- switch(as.character(exposure_rate),
                        "0" = 0.5,
                        "1" = 1.25,
                        "2" = 2.5)
  
  coefficient * (x - 1) * (x > 1)
  
}

.compute_rate_4 <- function(x, exposure_rate){
  
  coefficient <- switch(as.character(exposure_rate),
                        "0" = 2,
                        "1" = 5,
                        "2" = 10)
  
  intercept <- -0.25
  part_1 <- 0.25 / (5 / (sqrt(4 * coefficient + 1) + 1))
  part_2 <- (x - 5 / (sqrt(4 * coefficient + 1) + 1))^2
  
  intercept + part_1 / part_2
  
}
