get_catch = function(biomass_ts, hcr_option, thresholds, max_harvest, year) {
  
  b = biomass_ts[year,2] # get last year's biomass estimate
  
  h = NULL
  if (hcr_option == "1") {
    
    if (b <= thresholds$lower) {
      h = 0
    } else if (b <= thresholds$lower) {
      h = ((b - thresholds$lower)/(thresholds$upper - thresholds$lower))*max_harvest
    } else {
      h = max_harvest
    }
    
  } else if (hcr_option == "2") {
    
    if (b <= thresholds$lower) {
      h = 0
    } else {
      h = max_harvest
    }

  } else {
    stop("hcr_options must be 1 or 2")
  }
  
  catch = b*h
  
  out = list(
    catch = catch,
    h     = h
  )
  
  return(out)
  
}


# thresholds = list(
#   lower = 4000,
#   upper = 6000
# )

#get_catch(biomass_ts = biomass, hcr_option = "1", thresholds = thresholds, max_harvest = 0.1, year = 49)


