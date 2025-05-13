get_catch = function(curr_biomass, hcr_option, thresholds, max_harvest) {
  
  b = curr_biomass
  
  h = NULL
  if (hcr_option == "1") {
    # two-step rule
    if (b < thresholds$lower) {
      h = 0
    } else if (b >= thresholds$lower && b < thresholds$upper) {
      h = ((b - thresholds$lower)/(thresholds$upper - thresholds$lower))*max_harvest
    } else {
      h = max_harvest
    }
    
  } else if (hcr_option == "2") {
    # no linear scaling
    if (b <= thresholds$lower) {
      h = 0
    } else {
      h = max_harvest
    }

  } else if (hcr_option == "3") {
    
    if (hcr_option == "3" && length(thresholds) != 3) stop("Thresholds must be a list of length 3 for hcr_option 3")
    
  # three-step rule
    if (b <= thresholds$lower) {
      h = 0
    } else if (b >= thresholds$lower && b < thresholds$middle) {
      h = ((b - thresholds$lower)/(thresholds$upper - thresholds$lower))*max_harvest
    }
      
  } 
  
  else {
    stop("hcr_options must be 1, 2, 3, or 4")
  }
  
  catch = b*h
  
  out = list(
    catch = catch,
    h     = h
  )
  
  return(out)
  
}