library(devtools)
library(TMB)
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

                        ##### No fishing scenario #####

# load-in functions ------------------------------------------------------------
function_directory = file.path(here::here(), "R/")
function_files = list.files(function_directory)
for (i in 1:length(function_files)) {
  source(paste0(function_directory, function_files[i]))
}

# set up simulation ------------------------------------------------------------
n_sims = 500

thresholds = list(
  lower = 7000,
  upper = 12000 # does not matter when h = 0
)

settings = list(sim_years   = 60,
                par_list    = c("r", "K", "q"),
                thresholds  = thresholds,
                max_harvest = 0,
                hcr_option  = "1")

## get base model outputs
# make input data
data_directory = file.path(here::here(), "data/")
input_data     = make_input_data(data_directory = data_directory)

# fit
fit = fit.spict(input_data, verbose = TRUE, dbg = 0)

# run --------------------------------------------------------------------------

thresholds = list(
  lower = 7000,
  upper = 12000
)

settings = list(sim_years   = 60,
                par_list    = c("r", "K", "q"),
                thresholds  = thresholds,
                max_harvest = 0,
                hcr_option  = "1")

output = list()
for (i in 1:n_sims) {
  
  output[[i]] = tryCatch({
    
      run_simulation_2(settings = settings, data_directory = data_directory, estimation = FALSE, base_model_fit = fit)
    
    }, error = function(e) {
      
      message(paste("Error in simulation", i, ":", e$message))
      
    }
  )
  
}; message(paste("Completed", sum(sapply(output, Negate(is.null))), "out of", n_sims, "simulations successfully."))

# save simulation data
res_data_dir = file.path(here::here(), "res", "data", "rds/")
saveRDS(output, file = paste0(res_data_dir, "01_nofishing.rds"))

# process output ---------------------------------------------------------------
biomass_list = list()
for (i in 1:n_sims) biomass_list[[i]] = output[[i]]$biomass$absolute_biomass

biomass_list = Filter(Negate(is.null), biomass_list)
biomass_mat  = do.call(rbind, biomass_list)

biomass_df = data.frame(
  mu    = colMeans(biomass_mat, na.rm = TRUE),
  sigma = apply(biomass_mat, 2, sd)
) %>%
  mutate(phase = c(rep("Observed",49), rep("Simulated", 60))) %>%
  mutate(year = 1:(settings$sim_years + 49)+1972-1)

# save dataframe output
res_data_dir = file.path(here::here(), "res", "data", "dfs/")
write.csv(biomass_df, file = paste(res_data_dir, "01_nofishing.csv"))

# plot -------------------------------------------------------------------------
Bmsy = 5542.576768

biomass_df %>%
  
  ggplot(aes(x = year, y = mu)) +
  geom_line(linewidth = 1.2, aes(color = phase)) +
  geom_vline(xintercept = 2020.5, linewidth = 0.9, linetype = "dashed", color = "red") +
  geom_hline(yintercept = Bmsy, linetype = "dashed") +
  geom_ribbon(aes(ymin = mu - 4*sigma, ymax = mu + 4*sigma), alpha = 0.2) +
  custom_theme() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Biomass (t)") +
  annotate(geom="text", x = 1977, y = 6000, label = "B[msy]", parse = TRUE) +
  scale_color_manual(values = pnw_palette("Winter", 2)) +
  labs(title = "No fishing scenario")

fig_dir = file.path(here::here(), "res", "figures")
ggsave("nofishing.pdf", path = fig_dir)

