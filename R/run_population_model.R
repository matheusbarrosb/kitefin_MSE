run_population_model = function(catch, curr_biomass, pars, propag.uncert = TRUE, formulation = 'discrete') {
  
  if (propag.uncert == TRUE) {
    r = ifelse(rnorm(1, pars$r[1], pars$r[2]/2) <= 0, pars$r[1], rnorm(1, pars$r[1], pars$r[2]/2))
    K = rnorm(1, pars$K[1], pars$K[2]/2)
    m = rnorm(1, pars$m[1], pars$m[2])
    n = rnorm(1, pars$n[1], pars$n[2])
    sdb = rnorm(1, pars$sdb[1], pars$sdb[2])
  } else {
    r = pars$r[1]
    K = pars$K[1]
    m = pars$m[1]
    n = pars$n[1]
    sdb = pars$sdb[1]
  }

  proj_biomass = NA
  Ft = NA
  if (formulation == "discrete") {
    proj_biomass = curr_biomass + r*curr_biomass*(1 - (curr_biomass/K)) - catch
  } else if (formulation == "continuous") {
    Ft = catch/curr_biomass
    y = (n^(n/(n-1)))/(n-1)
    proj_biomass = (y*m*(curr_biomass/K) - ((y*m*(curr_biomass/K))^n) -  Ft*curr_biomass) + rnorm(1, 0, sdb)
  }
  
  return(proj_biomass)
  
}


#  
# par_list = c("r", "K", "MSYs", "q")
# pars = extract_pars(fit = fit, pars = par_list)
# 
# run_population_model(catch = 0, pars = pars, year = 49, curr_biomass = biomass[49,2])
