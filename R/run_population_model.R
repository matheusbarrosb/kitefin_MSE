run_population_model = function(catch, curr_biomass, pars, year, stochastic = TRUE) {
  
  if (stochastic == TRUE) {
    r = rnorm(1, pars$r[1], pars$r[2])
    K = rnorm(1, pars$K[1], pars$K[2])
  } else {
    r = pars$r
    K = pars$K
  }

  proj_biomass = curr_biomass + r*curr_biomass*(1 - (curr_biomass/K)) - catch
  
  return(proj_biomass)
  
}


# 
# par_list = c("r", "K", "MSYs", "q")
# pars = extract_pars(fit = fit, pars = par_list)
# 
# run_population_model(catch = 0, pars = pars, year = 49, biomass_ts = biomass)
