plot_BCRs = function(thresholds_list, bcr_names, max_harvest, ncol, palette) {
  b_max = max(sapply(thresholds_list, function(x) max(unlist(x)))) * 1.1
  b_vals = seq(0, b_max, length.out = 500)
  bcr_data = data.frame()
  
  for (bcr_name in bcr_names) {
    thresholds = thresholds_list[[bcr_name]]
    h_vals = numeric(length(b_vals))
    
    # Two-step HCR
    if (all(c("lower", "upper") %in% names(thresholds)) && !("middle" %in% names(thresholds))) {
      for (i in seq_along(b_vals)) {
        b = b_vals[i]
        if (b < thresholds$lower) {
          h_vals[i] = 0
        } else if (b >= thresholds$lower && b < thresholds$upper) {
          h_vals[i] = ((b - thresholds$lower) / (thresholds$upper - thresholds$lower)) * max_harvest
        } else if (b >= thresholds$upper) {
          h_vals[i] = max_harvest
        }
      }
    }
    # Three-step HCR
    else if (all(c("lower", "middle", "upper") %in% names(thresholds))) {
      plateau_harvest = ((thresholds$middle - thresholds$lower) / (thresholds$upper - thresholds$lower)) * max_harvest
      for (i in seq_along(b_vals)) {
        b = b_vals[i]
        if (b < thresholds$lower) {
          h_vals[i] = 0
        } else if (b >= thresholds$lower && b < thresholds$middle) {
          h_vals[i] = ((b - thresholds$lower) / (thresholds$middle - thresholds$lower)) * plateau_harvest
        } else if (b >= thresholds$middle && b < thresholds$upper) {
          h_vals[i] = plateau_harvest
        } else if (b >= thresholds$upper) {
          h_vals[i] = max_harvest
        }
      }
    }
    # Constant HCR
    else if (all(c("lower") %in% names(thresholds)) && length(thresholds) == 1) {
      for (i in seq_along(b_vals)) {
        b = b_vals[i]
        if (b < thresholds$lower) {
          h_vals[i] = 0
        } else {
          h_vals[i] = max_harvest
        }
      }
    }
    # Combine data
    bcr_data = rbind(bcr_data, data.frame(Biomass = b_vals, HarvestRate = h_vals, BCR = bcr_name)) # hcr_data
  }
  
  ggplot(bcr_data, aes(x = Biomass/1000, y = HarvestRate, color = BCR)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = pnw_palette(palette, n = length(bcr_names), type = "continuous")) +
    custom_theme() +
    facet_wrap(~ BCR, ncol = ncol) +
    labs(x = "Biomass (1000s of tonnes)",
         y = "Harvest Rate",
         color = "HCR") +
    theme(plot.title = element_text(face="bold", hjust=0.5),
          legend.position = "none")
}

# # Example usage:
# thresholds_list = list(
#   Standard_Two_step = list(lower=2771.289, upper=5542.577),
#   Precautionary_Two_step = list(lower=2771.289, upper=6928.222),
#   Hyper_precautionary_Two_step = list(lower=2771.289, upper=8313.866),
#   Standard_Three_step = list(lower=2771.289, middle=5542.577, upper=8313.866),
#   Precautionary_Three_step = list(lower=2771.289, middle=6928.222, upper=9699.51),
#   Hyper_precautionary_Three_step = list(lower=2771.289, middle=8313.866, upper=11085.15),
#   Constant = list(lower=5542.577)
# )
# 
# bcr_names = names(thresholds)
# 
# max_harvest = Umsy
# 
# plot_BCRs(thresholds_list, bcr_names, max_harvest, ncol = 2)
