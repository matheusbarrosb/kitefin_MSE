calc_rebuild_time = function(pars, uncertainty = TRUE, reps = 10000, plot = FALSE) {
  
  if (uncertainty) {
    t = rep(NA, reps)
    for (i in 1:reps) {
      
      r    = abs(rnorm(1, pars$r[1], pars$r[2]))
      Fm   = abs(rnorm(1, pars$Fm[1], pars$Fm[2]))
      Fmsy = abs(rnorm(1, pars$Fmsy[1], pars$Fmsy[2]))
      B    = abs(rnorm(1, pars$B[1], pars$B[2]))
      Bmsy = abs(rnorm(1, pars$Bmsy[1], pars$Bmsy[2]))
      
      Bratio = B / Bmsy
      Fratio = Fm / Fmsy
      
      denominator = 2 * (1 - Fm / r) - 1
      numerator = 2 * Bratio^-1 * (1 - Fm / r) - 1
      ratio = numerator / denominator
      
      if ((r - Fm) <= 0) {
        t[i] = NA
      } else {
        t[i] = 1 / (r - Fm) * log(ratio)
      }
    }
  } else {
    
    r    = pars$r[1]
    Fm   = pars$Fm[1]
    Fmsy = pars$Fmsy[1]
    B    = pars$B[1]
    Bmsy = pars$Bmsy[1]
    
    Bratio = B / Bmsy
    Fratio = Fm / Fmsy
    
    denominator = 2 * (1 - Fm / r) - 1
    numerator = 2 * Bratio^-1 * (1 - Fm / r) - 1
    ratio = numerator / denominator
    
  t = (1 / (r - Fm))
  }
  
  
  t = t[t>0]

  if (plot) plot(hist(t, breaks = 50, xlab = "Rebuild Time (years)", ylab = "Frequency"))
   
  return(t)
}
# 
# pars = list(
#   r    = c(get.par("r", fit)[2], get.par("r", fit)[4]),
#   Fm   = c(0.000000000001, 0.00000001), # Fm has to be small so the Fratio is not > r, which woulnd't allow rebuilding
#   Fmsy = c(get.par("Fmsyd", fit)[2], get.par("Fmsyd", fit)[4]/1000),
#   B    = c(biomass_df[51, 1], biomass_df[51, 1]),
#   Bmsy = c(get.par("Bmsyd", fit)[2], get.par("Bmsyd", fit)[4])
# )
# calc_rebuild_time(pars, uncertainty = TRUE, reps = 1000)
