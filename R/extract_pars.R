extract_pars = function(fit, pars) {

  ext_pars = list()
  for (i in 1:length(pars)) ext_pars[[i]] = get.par(paste0(pars[[i]]), fit, exp = FALSE)[,c("est","sd")]

  names(ext_pars) = pars  
  return(ext_pars)
    
}


#pars = c("r", "K", "MSYs", "q")
#a = extract_pars(fit = fit, pars = pars)
