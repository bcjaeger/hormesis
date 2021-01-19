##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_results
tabulate_sim <- function(sim_results) {

  sim_summary <- sim_results %>% 
    select(-seed) %>% 
    group_by(model, exposure_type, exposure_distribution, exposure_rate) %>% 
    summarize(across(everything(), median, na.rm = TRUE), .groups = 'drop') %>% 
    pivot_wider(names_from = model, values_from = AUC:cal_error)
  
  sim_summary

}
