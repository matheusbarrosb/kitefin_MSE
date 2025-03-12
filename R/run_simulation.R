run_simulation = function(settings, data_directory) {
  
  # load-in data
  data_directory = file.path(here::here(), "data/")
  input_data     = make_input_data(data_directory = data_directory)
  
  # get settings
  par_list = settings$par_list
  thresholds = settings$thresholds
  
  # allocate containers
  biomass = data.frame()
  
  for (t in 1:settings$sim_years) {
    
    if (t == 1) { # observed data only
      
      # fit model
      fit = fit.spict(input_data, verbose = FALSE)
      
      # get biomass estimates
      biomass = get.par('logB', fit, exp = TRUE)[,c("est","sd")]
      biomass = as.data.frame(biomass)
      biomass = rownames_to_column(biomass)
      names(biomass) = c("year", "est", "sd")
      biomass = biomass %>% filter(year == as.integer(year))
    
      # extract parameters
      pars = extract_pars(fit = fit, pars = par_list)
      
      # define next year's catch
      catch = get_catch(biomass_ts = biomass, hcr_option = "1", thresholds = thresholds, max_harvest = 0.1, year = dim(biomass)[1])
      
      # project population forward after fishery
      new_biomass        = run_population_model(catch = catch$catch, pars = pars, year = dim(biomass)[1], biomass_ts = biomass)
      new_biomass        = data.frame(as.numeric(biomass[dim(biomass)[1],1]) + t, new_biomass, 1)
      names(new_biomass) = names(biomass)
      biomass            = rbind(biomass, new_biomass)
      
      # simulate observation process
      surveys = sim_surveys(biomass_ts = biomass, pars = pars, year = dim(biomass)[1], stochastic = TRUE)
       
      # update data
      input_data = update_input_data(input_data = input_data, year = t, surveys = surveys, catch = catch)
      
    } else { # repeat for t > 1, estimation model uses observed + simulated data
      
      # fit model
      fit = fit.spict(input_data, verbose = FALSE)
      
      # get biomass estimates
      biomass = get.par('logB', fit, exp = TRUE)[,c("est","sd")] %>% as.data.frame()
      biomass = rownames_to_column(biomass)
      names(biomass) = c("year", "est", "sd")
      biomass = biomass %>% filter(year == as.integer(year))
      
      # extract parameters
      pars = extract_pars(fit = fit, pars = par_list)

      # define next year's catch
      catch = get_catch(biomass_ts = biomass, hcr_option = "1", thresholds = thresholds, max_harvest = 0.1, year = dim(biomass)[1])
      
      # project population forward after fishery
      new_biomass        = run_population_model(catch = catch$catch, pars = pars, year = dim(biomass)[1], biomass_ts = biomass)
      new_biomass        = data.frame(as.numeric(biomass[dim(biomass)[1],1]) + t, new_biomass, 1)
      names(new_biomass) = names(biomass)
      biomass            = rbind(biomass, new_biomass)
 
      # simulate observation process
      surveys = sim_surveys(biomass_ts = biomass, pars = pars, year = dim(biomass)[1] + t, stochastic = TRUE)
       
      # update data
      input_data = update_input_data(input_data = input_data, year = t, surveys = surveys, catch = catch)
      
    }
    
    print(paste0("simulation: ", t))
    
  }
  
  output = list(
    
    biomass    = biomass,
    input_data = input_data
    
  )
  
  return(output)
  
}

# thresholds = list(
#    lower = 2000,
#    upper = 6000
#  )
# 
# settings = list(sim_years = 3, par_list = c("r", "K", "q"), thresholds = thresholds)
# 
# run_simulation(settings = settings, data_directory = data_directory)










