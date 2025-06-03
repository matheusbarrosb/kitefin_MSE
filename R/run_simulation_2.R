run_simulation_2 = function(settings, data_directory, stochastic = TRUE, display_progress = TRUE, estimation = FALSE, base_model_fit) {
  
  # check settings object
  if (!is.list(settings)) stop("settings must be a list")
  
  # load-in data
  data_directory = file.path(here::here(), "data/")
  input_data     = make_input_data(data_directory = data_directory)
  
  # get settings
  par_list    = settings$par_list
  thresholds  = settings$thresholds
  
  # allocate containers
  est          = data.frame()
  biomass      = data.frame()
  est_biomass  = data.frame()
  true_biomass = matrix(NA, settings$sim_years, 3)
  real_catch   = matrix(NA, settings$sim_years, 2)
  
  if (estimation) {
  
    for (t in 1:settings$sim_years) {
      
      if (t == 1) { # conditions the operating model, observed data only
        
        ## ESTIMATION MODEL ------------------------------------------------------
        # fit model
        fit1 = fit.spict(input_data, verbose = FALSE)
        
        # get biomass estimates
        est_biomass = get.par('logB', fit1, exp = TRUE)[,c("est","cv")]
        est_biomass = as.data.frame(est_biomass)
        est_biomass = rownames_to_column(est_biomass); names(est_biomass) = c("year", "est", "cv")
        est_biomass = est_biomass %>% filter(year == as.integer(year))
        
        # extract parameters
        pars = extract_pars(fit = fit1, pars = par_list)
        
        # define next year's catch
        catch = get_catch(curr_biomass = est_biomass[49, 2], hcr_option = settings$hcr_option, thresholds = settings$thresholds, max_harvest = settings$max_harvest)
        real_catch[t,1] = catch$catch # store realized catch
        real_catch[t,2] = catch$h
        
        ## OPERATING MODEL -------------------------------------------------------
        # project population forward after fishery
        # stochastic = FALSE in the conditioning so the model starts off at the last estimated biomass
        true_biomass[t,2]  = run_population_model(catch = catch$catch, pars = pars, curr_biomass = est_biomass[dim(est_biomass)[1], 2], stochastic = FALSE) 

        new_biomass        = data.frame(as.numeric(est_biomass[dim(est_biomass)[1],1]) + t, true_biomass[t,2], 1)
        names(new_biomass) = names(est_biomass)
        biomass            = rbind(est_biomass, new_biomass)
        
        # simulate observation process
        if (stochastic == TRUE) {
          surveys = sim_surveys(curr_biomass = biomass[dim(biomass)[1],2], pars = pars, stochastic = TRUE)
        } else {
          surveys = sim_surveys(curr_biomass = biomass[dim(biomass)[1],2], pars = pars, stochastic = FALSE)  
        }
        
        # update data
        input_data = update_input_data(input_data = input_data, year = 1, surveys = surveys, catch = catch)
        
      } else { # repeat for t > 1, estimation model uses observed + simulated data
        
        ## ESTIMATION MODEL ------------------------------------------------------
        # run assessment model
        fit = fit.spict(input_data, verbose = FALSE)
        
        # get biomass estimates
        est = get.par('logB', fit, exp = TRUE)[,c("est","cv")] %>% as.data.frame()
        est = rownames_to_column(est)
        names(est) = c("year", "est", "cv")
        est = est %>% filter(year == as.integer(year))
        
        # extract model parameters
        # pars = extract_pars(fit = fit, pars = par_list)
        
        # define next year's catch (harvest control rule)
        catch = get_catch(curr_biomass = est[49 + t, 2], hcr_option = settings$hcr_option, thresholds = settings$thresholds, max_harvest = settings$max_harvest)
        real_catch[t,1] = catch$catch # store realized catch
        real_catch[t,2] = catch$h
        
        ## OPERATING MODEL -------------------------------------------------------
        # run population model
        if (stochastic == FALSE) {
          true_biomass[t,2] = run_population_model(catch = catch$catch, pars = pars, curr_biomass = true_biomass[t-1,2], stochastic = FALSE)
        } else {
          true_biomass[t,2] = run_population_model(catch = catch$catch, pars = pars, curr_biomass = true_biomass[t-1,2], stochastic = TRUE)
        }
        
        # run survey model
        if (stochastic == TRUE) {
          surveys = sim_surveys(curr_biomass = true_biomass[t,2], pars = pars, stochastic = TRUE)
        } else {
          surveys = sim_surveys(curr_biomass = true_biomass[t,2], pars = pars, stochastic = FALSE)
        }
        
        # update data
        input_data = update_input_data(input_data = input_data, year = t, surveys = surveys, catch = catch)
        
      }
      
      if (display_progress == TRUE) print(paste0("simulation ", t, " complete"))
      
    }
    
  } else {
    
    ## fully deterministic ##
    # fit1
    for (t in 1:settings$sim_years) {
      
      if (t == 1) { # conditions the operating model, observed data only
        
        # get biomass estimates
        est_biomass = get.par('logB', base_model_fit, exp = TRUE)[,c("est","cv")]
        est_biomass = as.data.frame(est_biomass)
        est_biomass = rownames_to_column(est_biomass)
        names(est_biomass) = c("year", "est", "cv")
        est_biomass = est_biomass %>% filter(year == as.integer(year))
        
        # extract parameters
        pars = extract_pars(fit = base_model_fit, pars = par_list)
        
        # define next year's catch
        # 3638.181 is the estimated 2020 biomass
        catch = get_catch(curr_biomass = 3638.181, hcr_option = settings$hcr_option, thresholds = thresholds, max_harvest = settings$max_harvest)
        real_catch[t,1] = catch$catch # store realized catch
        real_catch[t,2] = catch$h
        
        ## OPERATING MODEL -------------------------------------------------------
        # project population forward after fishery
        true_biomass[t,2]  = run_population_model(catch = catch$catch, pars = pars, curr_biomass = 3638.181, stochastic = FALSE) # stochastic = FALSE in the conditioning so the model starts off at the last estimated biomass
        
        new_biomass        = data.frame(as.numeric(est_biomass[dim(est_biomass)[1],1]) + t, true_biomass[t,2], 1)
        names(new_biomass) = names(est_biomass)
        biomass            = rbind(est_biomass, new_biomass)
        
        # simulate observation process
        if (stochastic == TRUE) {
          surveys = sim_surveys(curr_biomass = biomass[dim(biomass)[1],2], pars = pars, stochastic = TRUE)
        } else {
          surveys = sim_surveys(curr_biomass = biomass[dim(biomass)[1],2], pars = pars, stochastic = FALSE)  
        }
        
        # update data
        input_data = update_input_data(input_data = input_data, year = 1, surveys = surveys, catch = catch)
        
      } else { # repeat for t > 1, estimation model uses observed + simulated data
        
        # define next year's catch (harvest control rule)
        catch = get_catch(curr_biomass = true_biomass[t-1,2], hcr_option = settings$hcr_option, thresholds = thresholds, max_harvest = settings$max_harvest)
        real_catch[t,1] = catch$catch # store realized catch
        real_catch[t,2] = catch$h
        
        ## OPERATING MODEL -------------------------------------------------------
        # run population model
        if (stochastic == FALSE) {
          true_biomass[t,2] = run_population_model(catch = catch$catch, pars = pars, curr_biomass = est[dim(est)[1],2], stochastic = FALSE)
        } else {
          true_biomass[t,2] = run_population_model(catch = catch$catch, pars = pars, curr_biomass = true_biomass[t-1,2], stochastic = TRUE)
        }
        
        # run survey model
        if (stochastic == TRUE) {
          surveys = sim_surveys(curr_biomass = true_biomass[t,2], pars = pars, stochastic = TRUE)
        } else {
          surveys = sim_surveys(curr_biomass = true_biomass[t,2], pars = pars, stochastic = FALSE)
        }
        
        # update data
        input_data = update_input_data(input_data = input_data, year = t, surveys = surveys, catch = catch)
        
      }
      
      if (display_progress == TRUE) print(paste0("simulation ", t, " complete"))
      
    }
    
  }
  
  # MAKE OUTPUT ----------------------------------------------------------------
  
  # biomass trajectory
  true_biomass[,3]    = rep(0, nrow(true_biomass))
  true_biomass        = as.data.frame(true_biomass)
  names(true_biomass) = c("year", "est", "cv")
  true_biomass        = true_biomass %>% mutate(stage = "Simulated")
  
  est_biomass    = est_biomass %>% mutate(stage = "Observed")
  biomass_output = rbind(est_biomass, true_biomass)
  
  biomass_output$year = 1972:(2020 + settings$sim_years)
  
  # add relative biomass
  biomass_output$rel_biomass = biomass_output$est / base_model_fit$report$Bmsy
  
  names(biomass_output) = c("year", "absolute_biomass", "cv", "stage", "relative_biomass")
  
  output = list(
    biomass = biomass_output,
    catch   = real_catch,
    data    = input_data
  )
  
  return(output)
  
}
