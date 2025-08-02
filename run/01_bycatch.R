library(devtools)
library(spict)
library(readxl)
library(dplyr)
library(SPMpriors)
library(tibble)
library(utils)
library(ggplot2)
library(future)
library(future.apply)
library(progressr)
library(tidyr)
library(PNWColors)

##### Bycatch scenario #####

# load-in functions ------------------------------------------------------------
function_directory = file.path(here::here(), "R/")
function_files = list.files(function_directory)
for (i in 1:length(function_files)) {
  source(paste0(function_directory, function_files[i]))
}

# set up simulations -----------------------------------------------------------
## get base model outputs
# make input data
data_directory = file.path(here::here(), "data/")
input_data     = make_input_data(data_directory = data_directory)

# fit
fit = fit.spict(input_data, verbose = TRUE, dbg = 0);plot(fit)

# reference points
Bmsy = get.par("Bmsys", fit)[2];print(paste0("Bmsy = ", Bmsy))
Fmsy = get.par("Fmsys", fit)[2];print(paste0("Fmsy = ", Fmsy))
Umsy = 1 - exp(-Fmsy)

# setup BCR settings
BCR_type = rep(c("Standard", "Precautionary", "Hyper-precautionary"), 2)
BCR_cat  = c(rep("Two_step", 3), rep("Three_step", 3))
BCRs     = paste0(BCR_type, "_", BCR_cat)
N_BCRs   = length(BCRs)
option   = c(rep("1", N_BCRs/2), rep("3", N_BCRs/2))

# add constant
BCRs     = c(BCRs, "Constant")
BCR_cat  = c(BCR_cat, "Constant")
option   = c(option, "2")
N_BCRs   = length(BCRs) # increment

# setup thresholds
lwr = rep(0.5*Bmsy, N_BCRs) # 20% of K
mid = c(NA, NA, NA, Bmsy, 1.25*Bmsy, 1.5*Bmsy)
upr = c(Bmsy, 1.25*Bmsy, 1.5*Bmsy, 1.5*Bmsy, 1.75*Bmsy, 2*Bmsy)

thresholds = list()
for (i in 1:N_BCRs) {
  if (BCR_cat[i] == "Two_step") {
    thresholds[[i]] = list(lower = lwr[i], upper = upr[i])
  } else if (BCR_cat[i] == "Three_step") {
    thresholds[[i]] = list(lower = lwr[i], middle = mid[i], upper = upr[i])
  }
};thresholds[[N_BCRs]] = list(lower = Bmsy)
names(thresholds) = BCRs

palette = "Bay"

plot_BCRs(thresholds, BCRs, Umsy, ncol = 2, palette)

fig_dir = file.path(here::here(), "res", "figures")
ggsave("BCRs.pdf", path = fig_dir)

# allocate containers
settings = list()
output_list = list() # this is the main aggregated output
output = list() # for the internal loop over replicate simulations

# run --------------------------------------------------------------------------
n_sims = 150
sim_years = 75

parallel = TRUE
estimation = TRUE

if (parallel) {
  library(future.apply)
  
  n_cores = parallel::detectCores() - 1
  plan(multisession, workers = n_cores) # Use "multicore" instead if on Linux/Mac
  
  output_list = list()
  
  for (k in 1:N_BCRs) {
    settings[[k]] <- list(
      formulation = "continuous",
      sim_years   = sim_years,
      par_list    = c("r", "K", "q", "m", "n", "sdb"),
      thresholds  = thresholds[[k]],
      max_harvest = Umsy, 
      hcr_option  = option[k],
      estimation  = estimation
    )
    
    output <- future_lapply(1:n_sims, function(i) {
      tryCatch({
        run_simulation_2(
          settings = settings[[k]],
          data_directory = data_directory,
          estimation = settings[[k]]$estimation,
          base_model_fit = fit
        )
      }, error = function(e) {
        message(paste("Error in simulation", i, ":", e$message))
        NULL
      })
    })
    
    message(paste("Completed", sum(sapply(output, Negate(is.null))), "out of", n_sims, "simulations successfully."))
    
    output_list[[k]] = output
    
  }
} else {
  
  for (k in 1:N_BCRs) {
    
    settings[[k]] = list(formulation = "continuous",
                         sim_years   = sim_years,
                         par_list    = c("r", "K", "q", "m", "n", "sdb"),
                         thresholds  = thresholds[[k]],
                         max_harvest = Umsy, 
                         hcr_option  = option[k],
                         estimation  = estimation)
    
    for (i in 1:n_sims) {
      
      output[[i]] = tryCatch({
        
        run_simulation_2(settings = settings[[k]], data_directory = data_directory, estimation = settings[[k]]$estimation, base_model_fit = fit)
        
      }, error = function(e) {
        
        message(paste("Error in simulation", i, ":", e$message))
        NULL
      }
      )
      
    }; message(paste("Completed", sum(sapply(output, Negate(is.null))), "out of", n_sims, "simulations successfully."))
    
    output_list[[k]] = output
    
  }; names(output_list) = paste0(BCRs)
  
} # end of simulations -----------------------------------------------------------

# save .rds file
res_data_dir = file.path(here::here(), "res", "data", "rds/")
saveRDS(output_list, file = paste0(res_data_dir, "bycatch.rds"))




