plot_hcr = function(hcr_option, max_harvest, thresholds, title) {
  
  h = rep(NA, 10000)
  b = seq(3000, 20000, length.out = 10000)
  
  if (hcr_option == "1") {
    
    for(i in 1:length(b)) {
      
      if (b[i] <= thresholds$lower) {
          
        h[i] = 0
          
      } else if (b[i] > thresholds$lower && b[i] < thresholds$upp) {
          
          h[i] = ((b[i] - thresholds$lower)/(thresholds$upper - thresholds$lower))*max_harvest
          
        } else {h[i] = max_harvest}
        
    }  
    
  } else if (hcr_option == "2") {
    
    for(i in 1:length(b)) {
    
      if (b[i] <= thresholds$lower) {
        
        h[i] = 0
        
      } else {
        
        h[i] = max_harvest
        
        }

    }
      
  } else {
    stop("Invalid HCR option. Choose '1' or '2'.")
  }

  # PLOTTING
  plot(b, h, type = "l", xlab = "Biomass (t)", ylab = "Harvest rate", main = paste0(title), col = "blue")
      
}  


# Run the function

#plot_hcr(hcr_option = "1", max_harvest = 0.02, thresholds = list(lower = 0.8*7000, upper = 1.2*12000))





    
    
