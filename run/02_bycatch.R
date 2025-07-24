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

# set up simulation ------------------------------------------------------------
n_sims = 100

Bmsy = 5914.2806440

lwr = Bmsy
upr = 1.5*Bmsy

thresholds = list(
  lower = lwr,
  upper = upr 
)

h = as.numeric(seq(0.005, 0.05, by = 0.005))
settings = list()
output_list = list()
output = list()
for (k in 1:length(h)) {

  settings[[k]] = list(formulation = "continuous",
                       sim_years   = 75,
                       par_list    = c("r", "K", "q", "m", "n", "sdb"),
                       thresholds  = thresholds,
                       max_harvest = h[k], 
                       hcr_option  = "1",
                       estimation  = FALSE)
  
  for (i in 1:n_sims) {
    
    output[[i]] = tryCatch({
      
      run_simulation_2(settings = settings[[k]], data_directory = data_directory, estimation = settings[[k]]$estimation, base_model_fit = fit)
      
    }, error = function(e) {
      
      message(paste("Error in simulation", i, ":", e$message))
      
    }
    )
    
  }; message(paste("Completed", sum(sapply(output, Negate(is.null))), "out of", n_sims, "simulations successfully."))
  
  output_list[[k]] = output; names(output_list)[k] = paste0("h_", h[k])
  
}

# save .rds file
res_data_dir = file.path(here::here(), "res", "data", "rds/")
saveRDS(output_list, file = paste0(res_data_dir, "bycatch_dt.rds"))

# process output ---------------------------------------------------------------
if (!exists("output_list")) {
  res_data_dir = file.path(here::here(), "res", "data", "rds/")
  output_list  = readRDS(file = paste0(res_data_dir, "bycatch_dt.rds"))
}

biomass_df_list = list()
catch_df_list   = list()
for (i in 1:length(output_list)) {
  biomass      = lapply(output_list[[i]], function(x) x$biomass$absolute_biomass)
  biomass_mat  = do.call(rbind, biomass)
  biomass_mean = apply(biomass_mat, 2, mean)
  biomass_sd   = apply(biomass_mat, 2, sd)
  
  catch       = lapply(output_list[[i]], function(x) x$catch[,1])
  catch_mat   = do.call(rbind, catch)
  catch_mean  = apply(catch_mat, 2, mean)
  catch_sd    = apply(catch_mat, 2, sd)
  
  biomass_df = data.frame(
    year    = seq(1972, 2020 + settings[[i]]$sim_years),
    biomass = biomass_mean,
    sd      = biomass_sd
  )
  
  catch_df = data.frame(
    year   = seq(2020, 2020 + settings[[i]]$sim_years-1),
    catch  = catch_mean,
    sd     = catch_sd
  )

  biomass_df_list[[i]] = biomass_df %>%
    mutate(h = rep(h[i], nrow(biomass_df)))
  
  catch_df_list[[i]] = catch_df %>%
    mutate(h = rep(h[i], nrow(catch_df)))
  
}

# summarize
biomass_df = do.call(rbind, biomass_df_list)

biomass_df %>%
  group_by(as.factor(h)) %>%
  summarise(
    mean_biomass = mean(biomass),
    sd_biomass   = sd(biomass)
  )

# rbind all biomass dataframes and mutate a h column
biomass_df = do.call(rbind, biomass_df_list)
biomass_df = biomass_df %>%
  mutate(h = rep(h, each = nrow(biomass_df_list[[1]])))
  
biomass_df %>%
  ggplot(aes(x = year, y = biomass, group = h, color = h)) +
  geom_line() +
  custom_theme() +
  theme(legend.position = "right") +
  geom_hline(yintercept = Bmsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "black") +
  annotate("text", x = 2000, y = 6400, label = "italic(B[msy])", parse = TRUE, color = "black") +
  ylab("Biomass (t)") +
  xlab("Year") +
  ylim(0,11000) 

pars = list(
  r    = c(get.par("r", fit)[2], 0),
  Fm   = c(0.02, 0),
  Fmsy = c(get.par("Fmsyd", fit)[2], 0),
  B    = c(3800, 10),
  Bmsy = c(get.par("Bmsyd", fit)[2], 0)
)

ts = calc_rebuild_time(pars, uncertainty = TRUE, reps = 10000)
median(ts, na.rm = TRUE)

mean(calc_rebuild_time(pars, uncertainty = TRUE, reps = 100000), na.rm = TRUE)
  
# Calculate rebuilding times ---------------------------------------------------
# This is preliminary, need to determine range of harvest rates

h = seq(0.0001, 0.025, by = 0.00002)
t.mu = rep(NA, length(h))
t.sd = rep(NA, length(h))
K = length(h)
N = 1000

for (k in 1:K) {
  
  pars = list(
    r    = c(get.par("r", fit)[2], 0.02), # could be deterministic for r and K, vary biomass only?
    Fm   = c(h[k], 0),
    Fmsy = c(0.2, 0.001),
    B    = c(biomass_df[51, 1], 200),
    Bmsy = c(get.par("Bmsyd", fit)[2], 200)
  )
  
  t.mu[k] = median(calc_rebuild_time(pars, uncertainty = TRUE, reps = N), na.rm = TRUE)
  t.sd[k] = sd(calc_rebuild_time(pars, uncertainty = TRUE, reps = N), na.rm = TRUE)
  
}


data.frame(
  h,
  t.mu,
  t.sd
) %>%
  ggplot(aes(x = h, y = t.mu)) +
  geom_line() +
  geom_ribbon(aes(ymin = t.mu - t.sd, ymax = t.mu + t.sd), alpha = 0.2) +
  custom_theme() +
  xlab("Harvest rate") +
  ylab("Rebuilding time (years)") +
  xlim(c(0, 0.025)) +
  ylim(c(0,200))
  



