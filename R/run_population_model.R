run_population_model = function(catch, curr_biomass, pars, year, stochastic = TRUE) {
  
  if (stochastic == TRUE) {
    r = ifelse(rnorm(1, pars$r[1], pars$r[2]) <= 0, pars$r[1], rnorm(1, pars$r[1], pars$r[2]))
    K = rnorm(1, pars$K[1], pars$K[2])
  } else {
    r = pars$r[1]
    K = pars$K[2]
  }

  proj_biomass = curr_biomass + r*curr_biomass*(1 - (curr_biomass/K)) - catch
  
  return(proj_biomass)
  
}


# 
# par_list = c("r", "K", "MSYs", "q")
# pars = extract_pars(fit = fit, pars = par_list)
# 
# run_population_model(catch = 0, pars = pars, year = 49, biomass_ts = biomass)
