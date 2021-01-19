##' Primary simulation function for analysis or hormesis
##'
##' the objective of the simulation is to describe goodness-of-fit
##' for restricted cubic splines (and other formulations) when
##' the data are generated under the hockey stick pattern. 
##'
##' @title Hormesis simulation
##' 
##' @param n_obs number of observations
##' @param dist distribution of the exposure. Valid options and descriptions:
##'  - 'exp': exponential 
##'  - 'gam': inverse gaussian
##'  - 'log': exponentiated gaussian (??)
##'  - 'uni': uniform
##'  - 'nor': ??
##' @param type type of the exposure. Valid options and descriptions:
##'  - 1: ??
##'  - 2: ??
##'  - 3: ??
##' @param rate rate of the relationship between the exposure and outcome. Valid options and descriptions:
##'  - 1: ??
##'  - 2: ??
##' 
hormesis_sim <- function(n_obs,
                         exposure_distribution,
                         exposure_type,
                         exposure_rate) {
  
  x <- switch(
    toupper(exposure_distribution),
    "EXP" = rexp(n = n_obs, rate = 1/2),
    "GAM" = rinvgauss(n = n_obs, mean = 1, shape = 0.4),
    "LOG" = exp(rnorm(n = n_obs)),
    "UNI" = runif(n = n_obs, min = 0, max = 5),
    "NOR" = rnor(n = n_obs, mean = 3, sd = 1, low_threshold = 0)
  )
  
  rates <- compute_rate(x = x, 
                        exposure_type = exposure_type, 
                        exposure_rate = exposure_rate)
  
  y_alpha <- 0.1656439
  y_lambda <- 0.009788171
  y <- (1 / y_alpha) * 
    log(1 - y_alpha * log(runif(n_obs)) / (y_lambda * exp(rates)))
  
  cen_alpha <- 0.1420253
  cen_lambda <- 0.01048467
  cen <- (1 / cen_alpha) * 
    log(1 - cen_alpha * log(runif(n_obs)) / (cen_lambda * exp(1)))
  
  ycen <- pmin(y, cen)
  di <- as.numeric(y <= cen)
  
  tibble(time = ycen, status = di, exposure = x, rate = rates)
  
}

