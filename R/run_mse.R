run_mse = function(settings, data_directory, n_sims = 1000, seed = 1) {
  
  monitor = matrix(NA, nrow = 49 + settings$sim_years, ncol = n_sims)
  
  # run simulations
  for (i in 1:n_sims) {
    
    set.seed(seed + i)
    
    monitor[,i] = run_simulation(settings = settings, data_directory = data_directory)$biomass[,2]
    
  }
  
  # compute results
  means = rowMeans(monitor)
  sds   = apply(monitor, 1, sd, na.rm=TRUE)/2
  year  = 1972:(1972 + 49 + (settings$sim_years - 1))
  
  output = data.frame(means, sds, year)
  
  
  
}

# monitor = matrix(NA, nrow = 49 + settings$sim_years, ncol = n_sims)
# for (i in 1:n_sims) {
#   
#   monitor[,i] = run_simulation(settings = settings, data_directory = data_directory)$biomass[,2]
#   
# }