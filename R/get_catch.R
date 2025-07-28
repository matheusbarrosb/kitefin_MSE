get_catch = function(curr_biomass, hcr_option, thresholds, max_harvest) {
  
  b = curr_biomass
  
  h = NULL
  if (hcr_option == "1") {
    # two-step rule
    if (is.null(thresholds$lower) || is.null(thresholds$upper)) {
      stop("For hcr_option 1, thresholds must include lower and upper values.")
    }
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
    if (is.null(thresholds$lower) || is.null(thresholds$middle) || is.null(thresholds$upper)) {
      stop("For hcr_option 3, thresholds must include lower, middle, and upper values.")
    }
    
    plateau_harvest = ((thresholds$middle - thresholds$lower)/(thresholds$upper - thresholds$lower)) * max_harvest
    if (b < thresholds$lower) {
      h = 0
    } else if (b >= thresholds$lower && b < thresholds$middle) {
      h = ((b - thresholds$lower) / (thresholds$middle - thresholds$lower)) * plateau_harvest
    } else if (b >= thresholds$middle && b < thresholds$upper) {
      h = plateau_harvest
    } else if (b >= thresholds$upper) {
      h = max_harvest
    }

  } 
  
  else {
    stop("hcr_options must be 1, 2, or 3")
  }
  
  catch = b*h
  
  if (catch > 151.830442) { # cap catch at MSY
    catch = 151.830442
  }
  
  out = list(
    catch = catch,
    h     = h
  )
  
  return(out)
  
}
