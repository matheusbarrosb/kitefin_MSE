sim_surveys = function(biomass_ts, pars, stochastic = TRUE, year) {
  
  # get catchabilities
  if (stochastic == TRUE) {
    q1 = abs(rnorm(1, pars$q[1,1], pars$q[1,2]))
    q2 = abs(rnorm(1, pars$q[2,1], pars$q[2,2]))
    q3 = abs(rnorm(1, pars$q[3,1], pars$q[3,2]))
    q4 = abs(rnorm(1, pars$q[4,1], pars$q[4,2]))
  } else {
    q1 = pars$q[1,1]
    q2 = pars$q[2,1]
    q3 = pars$q[3,1]
    q4 = pars$q[4,1]
  }
  
  # get last year's biomass
  curr_biomass = as.numeric(biomass_ts[dim(biomass_ts)[1], 2])
  
  # gillnet
  I1 = q1 * curr_biomass
  
  # handline
  I2 = q2 * curr_biomass
  
  # DCF 1
  I3 = q3 * curr_biomass
  
  # DCF 2
  I4 = q4 * curr_biomass
  
  output = list(I1 = I1, I2 = I2, I3 = I3, I4 = I4)
 
  return(output)
   
}
# 
# par_list = c("r", "K", "MSYs", "q")
# pars = extract_pars(fit = fit, pars = par_list)
# 
# sim_surveys(biomass_ts = biomass, pars = pars, year = 49, stochastic = FALSE)
