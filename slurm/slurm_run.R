
## library() calls go here

library(riskRegression, quietly = TRUE)
library(conflicted, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(survival, quietly = TRUE)
library(statmod, quietly = TRUE)
library(splines, quietly = TRUE)
library(glue, quietly = TRUE)
library(rms, quietly = TRUE)

conflict_prefer("filter",    "dplyr")
conflict_prefer("slice",     "dplyr")
conflict_prefer("select",    "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("gather",    "tidyr")
conflict_prefer("set_names", "purrr")
conflict_prefer("plan",      "drake")

R.utils::sourceDirectory("R")

.rslurm_func <- function(seed,
                         exposure_distribution = 'exp',
                         exposure_type = 1,
                         exposure_rate = 1,
                         n_trn = 1000,
                         n_tst = 10000,
                         n_run = 10) {
  
  set.seed(seed)
  
  results <- tibble(model = character(),
                    AUC = double(),
                    IPA = double(),
                    cal_error = double())
  
  n_obs = n_trn + n_tst
  
  for(i in seq(n_run)) {
    
    data_sim <- hormesis_sim(n_obs = n_obs, 
                             exposure_distribution = exposure_distribution, 
                             exposure_type = exposure_type,
                             exposure_rate = exposure_rate)
    
    # ggplot(data_sim, aes(x = exposure, y = rate)) +
    #   geom_line()
    
    trn_index <- sample(x = n_obs, size = n_trn)
    
    data_trn <- data_sim[trn_index, ]
    data_tst <- data_sim[-trn_index, ]
    
    fit_rcs <- coxph(Surv(time, status) ~ rcs(exposure),
                     data = data_trn, 
                     x = TRUE, 
                     y = TRUE)
    
    # oracle model
    
    fit_oracle <- switch(
      as.character(exposure_type),
      '1' = coxph(Surv(time, status) ~ pol(exposure),
                  data = data_trn, 
                  x = TRUE, 
                  y = TRUE),
      '2' = coxph(Surv(time, status) ~ exposure,
                  data = data_trn, 
                  x = TRUE, 
                  y = TRUE),
      '3' = coxph(Surv(time, status) ~ lsp(exposure, 1),
                  data = data_trn, 
                  x = TRUE, 
                  y = TRUE),
      '4' = coxph(Surv(time, status) ~ lsp(exposure, c(1.1, 1.2, 1.30)),
                  data = data_trn, 
                  x = TRUE, 
                  y = TRUE)
    )
    
    # plot(Predict(fit_rcs, exposure))
    # plot(Predict(fit_oracle, exposure))
    
    predicted_risk_times <- quantile(
      data_sim$time,
      probs = seq(0.10, 0.90, by = 0.10)
    )
    
    predicted_risk <- map(
      list(rcs = fit_rcs, oracle = fit_oracle),
      predictRisk,
      newdata = data_tst, 
      times = predicted_risk_times
    )
    
    sc <- Score(
      object = predicted_risk,
      data = data_tst,
      formula = Surv(time, status) ~ 1, 
      times = predicted_risk_times,
      summary = c("IPA", 'ibs'),
      plots = 'calibration',
      se.fit = 0
    )
    
    auc <- sc$AUC$score %>% 
      group_by(model) %>% 
      summarize(AUC = mean(AUC))
    
    ipa <- sc$Brier$score %>% 
      filter(model != 'Null model') %>% 
      group_by(model) %>%
      summarize(IPA = mean(IPA))
    
    cal_plot <- try(plotCalibration(sc, 
                                times = max(predicted_risk_times),
                                plot = FALSE, 
                                cens.method = 'local'))
    
    if(inherits(cal_plot, 'try-error')){
      cal_error <- tibble(
        model = c('rcs', 'oracle'),
        cal_error = c(NA_real_, NA_real_)
      )
    } else {
      cal_error <- cal_plot$plotFrames %>% 
        map_dfr(summarize, cal_error = mean((Pred-Obs)^2, na.rm = TRUE),
                .id = 'model')
    }
    
    results <- results %>% 
      add_row(
        reduce(
          .x = list(auc, ipa, cal_error), 
          .f = left_join
        )
      )
    
  }
  
  results %>% 
    mutate(
      seed = seed,
      exposure_distribution = exposure_distribution,
      exposure_type = exposure_type,
      exposure_rate = exposure_rate
    )
  
}

.rslurm_params <- expand.grid(
  seed = 1:100,
  exposure_distribution = c('exp','gam','log','uni','nor'),
  exposure_type = c(1,2,3,4),
  exposure_rate = c(0,1,2)
)

.rslurm_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
.rslurm_istart <- .rslurm_id * 1 + 1
.rslurm_iend <- min((.rslurm_id + 1) * 1, nrow(.rslurm_params))

.rslurm_result <- do.call(
  what = parallel::mcmapply,
  args = c(
    FUN = .rslurm_func,
    .rslurm_params[.rslurm_istart:.rslurm_iend, , drop = FALSE],
    mc.cores = 1,
    mc.preschedule = TRUE,
    SIMPLIFY = FALSE
  )
)

saveRDS(
  .rslurm_result, 
  file = glue('results/output_{.rslurm_id}.RDS')
)
