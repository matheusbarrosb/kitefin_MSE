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
library(doFuture)

##### Hyper-precautionary two-step scenario #####

# load-in functions ------------------------------------------------------------
function_directory = file.path(here::here(), "R/")
function_files = list.files(function_directory)
for (i in 1:length(function_files)) {
  source(paste0(function_directory, function_files[i]))
}

# set up simulation ------------------------------------------------------------
n_sims = 200

Bmsy = 5542.577 # from stochastic reference points
Fmsy = 0.027491
Umsy = 1 - exp(-Fmsy) # converted from instantaneous to harvest rate

thresholds = list(
  lower = 1.2 * Bmsy,
  upper = 1.5 * Bmsy
)

settings = list(sim_years   = 75,
                par_list    = c("r", "K", "q"),
                thresholds  = thresholds,
                max_harvest = Umsy, # 1 - exp(Fmsy)
                hcr_option  = "1")

## get base model outputs
# make input data
data_directory = file.path(here::here(), "data/")
input_data     = make_input_data(data_directory = data_directory)

# fit
fit = fit.spict(input_data, verbose = TRUE, dbg = 0);plot(fit)

# deterministic simulations ----------------------------------------------------
det = list()
for (i in 1:n_sims) {

  det[[i]] = run_simulation_2(
    settings       = settings,
    data_directory = data_directory,
    estimation     = FALSE,
    base_model_fit = fit
  )
  
}

# run --------------------------------------------------------------------------

# Parallel setup
library(doFuture)
library(foreach)

plan(multisession, workers = parallel::detectCores() - 1)
print(paste0("Allocating ", parallel::detectCores() - 1, " cores"))

# Run simulations in parallel
output = foreach(i = 1:n_sims) %dofuture% {
  tryCatch({
    run_simulation_2(
      settings       = settings,
      data_directory = data_directory,
      estimation     = TRUE,
      base_model_fit = fit
    )
  }, error = function(e) {
    message(paste("Error in simulation", i, ":", e$message))
    NULL  # Return NULL for failed simulations
  })
}
message(paste("Completed", sum(sapply(output, Negate(is.null))), "out of", n_sims, "simulations successfully."))

# save simulation data
res_data_dir = file.path(here::here(), "res", "data", "rds/")
saveRDS(output, file = paste0(res_data_dir, "05_hyper_precaut_twostep.rds"))

# process output ---------------------------------------------------------------
biomass_list = list()
for (i in 1:n_sims) biomass_list[[i]] = output[[i]]$biomass$absolute_biomass

biomass_list = Filter(Negate(is.null), biomass_list)
biomass_mat  = do.call(rbind, biomass_list)

biomass_df = data.frame(
  mu    = colMeans(biomass_mat, na.rm = TRUE),
  sigma = apply(biomass_mat, 2, sd)
) %>%
  mutate(phase = c(rep("Observed",49), rep("Simulated", settings$sim_years))) %>%
  mutate(year = 1:(settings$sim_years + 49)+1972-1)

# save dataframe output
res_data_dir = file.path(here::here(), "res", "data", "dfs/")
write.csv(biomass_df, file = paste(res_data_dir, "05_hyper_precaut_twostep.csv"))

# plot -------------------------------------------------------------------------
Bmsy = 5542.576768

biomass_df %>%
  
  ggplot(aes(x = year, y = mu)) +
  geom_line(linewidth = 1.2, aes(color = phase)) +
  geom_vline(xintercept = 2020.5, linewidth = 0.9, linetype = "dashed", color = "red") +
  geom_hline(yintercept = Bmsy, linetype = "dashed") +
  geom_ribbon(aes(ymin = mu - 2*sigma, ymax = mu + 2*sigma), alpha = 0.2) +
  custom_theme() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Biomass (t)") +
  annotate(geom="text", x = 1977, y = 6000, label = "B[msy]", parse = TRUE) +
  scale_color_manual(values = pnw_palette("Winter", 2)) +
  labs(title = "Hyper-precautionary two-step HCR")

fig_dir = file.path(here::here(), "res", "figures")
ggsave("hyper_precaut_twostep.pdf", path = fig_dir)

