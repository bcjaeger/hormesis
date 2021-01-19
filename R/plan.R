
the_plan <-
  drake_plan(
    
    # note: R scripts in slurm/ are used to create sim/sim_results.rds, 
    # which is the primary data for this analysis plan. 
    sim_results = load_sim(file = 'sim/sim_results.rds'),
    tbl_sim = tabulate_sim(sim_results)
    
  )
