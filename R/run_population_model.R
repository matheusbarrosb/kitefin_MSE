run_population_model = function(catch, curr_biomass, pars, propag.uncert = TRUE, formulation = 'discrete') {
  
  if (propag.uncert == TRUE) {
    r = ifelse(rnorm(1, pars$r[1], pars$r[2]/2) <= 0, pars$r[1], rnorm(1, pars$r[1], pars$r[2]/2))
    K = rnorm(1, pars$K[1], pars$K[2]/2)
    m = rnorm(1, pars$m[1], pars$m[2])
    n = rnorm(1, 1.821316, 0.1)
    sdb = rnorm(1, pars$sdb[1], pars$sdb[2])
  } else {
    r = pars$r[1]
    K = pars$K[1]
    m = pars$m[1]
    n = 1.821316 # for some reason 'the get.pars' function from SPiCT returns NA for n, so I'm fixing the value
    sdb = pars$sdb[1]
  }

  proj_biomass = NA
  Ft = NA
  if (formulation == "discrete") {
    proj_biomass = curr_biomass + r*curr_biomass*(1 - (curr_biomass/K)) - catch
  } else if (formulation == "continuous") {
    if (catch == 0) {
    Ft = 0
  }
    else {
      Ft = catch/curr_biomass
    }
    
    gamma = n^(n/(n-1)) / (n - 1)
    m = r * K / n^(n/(n-1))
    
    term1 = gamma * m * curr_biomass / K
    term2 = gamma * m * (curr_biomass / K)^n
    term3 = Ft * curr_biomass
    
    proj_biomass = curr_biomass + (term1 - term2 - term3) #+ rnorm(1, 0, sdb)
  
  }
  
  return(proj_biomass)
  
}


