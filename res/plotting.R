# load data
data_directory = file.path(here::here(), "res", "data", "rds")
rds_files      = list.files(path = data_directory, pattern = "\\.rds$", full.names = TRUE)
data_list      = lapply(rds_files, readRDS)

clean_names = sub("^\\d+[_-]*", "", tools::file_path_sans_ext(basename(rds_files)))

names(data_list) = clean_names

# wrangle data
for (k in 1:length(data_list)) {
  data_list[[k]] = Filter(function(x) !is.null(x$biomass), data_list[[k]])
}

nofishing_bmat = matrix(NA, nrow = 109, ncol = length(data_list[[1]]))
for (j in 1:length(data_list[[1]])) {
  for (i in 1:109) nofishing_bmat[i, j] = data_list[[1]][[j]]$biomass[i,2]
}
year  = 1:109 + 1980 
mu    = rowMeans(nofishing_bmat, na.rm = TRUE)
sigma = apply(nofishing_bmat, 1, sd, na.rm = TRUE)

nofishing_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "No fishing")

bycatch_bmat = matrix(NA, nrow = 109, ncol = length(data_list[[2]]))
for (j in 1:length(data_list[[2]])) {
  for (i in 1:109) bycatch_bmat[i, j] = data_list[[2]][[j]]$biomass[i,2]
}
mu    = rowMeans(bycatch_bmat, na.rm = TRUE)
sigma = apply(bycatch_bmat, 1, sd, na.rm = TRUE)

bycatch_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Bycatch")

twostep_bmat = matrix(NA, nrow = 124, ncol = length(data_list[[3]]))
for (j in 1:length(data_list[[3]])) {
  for (i in 1:124) twostep_bmat[i, j] = data_list[[3]][[j]]$biomass[i,2]
}
year = 1:124 + 1980
mu    = rowMeans(twostep_bmat, na.rm = TRUE)
sigma = apply(twostep_bmat, 1, sd, na.rm = TRUE)

twostep_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Two-step HCR")

precaut_twostep_bmat = matrix(NA, nrow = 124, ncol = length(data_list[[4]]))
for (j in 1:length(data_list[[4]])) {
  for (i in 1:124) precaut_twostep_bmat[i, j] = data_list[[4]][[j]]$biomass[i,2]
}
mu    = rowMeans(precaut_twostep_bmat, na.rm = TRUE)
sigma = apply(precaut_twostep_bmat, 1, sd, na.rm = TRUE)

precaut_twostep_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Precautionary two-step HCR")

hyper_precaut_twostep_bmat = matrix(NA, nrow = 124, ncol = length(data_list[[5]]))
for (j in 1:length(data_list[[5]])) {
  for (i in 1:124) hyper_precaut_twostep_bmat[i, j] = data_list[[5]][[j]]$biomass[i,2]
}
mu    = rowMeans(hyper_precaut_twostep_bmat, na.rm = TRUE)
sigma = apply(hyper_precaut_twostep_bmat, 1, sd, na.rm = TRUE)

hyper_precaut_twostep_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Hyper-precautionary two-step HCR")


bio_df = rbind(nofishing_df, bycatch_df, twostep_df,
               precaut_twostep_df, hyper_precaut_twostep_df)

bio_df$phase = ifelse(bio_df$sigma == 0, "Observed", "Simulated")

Bmsy = 5542
bio_df %>%
  ggplot(aes(x = year, y = mu)) +
  geom_line(aes(color = Scenario, linetype = phase)) +
  geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma, fill = Scenario), alpha = 0.15) +
  geom_hline(yintercept = Bmsy, linetype = "dashed") +
  custom_theme() +
  theme(legend.position = "right") +
    scale_color_manual(values = pnw_palette("Bay", 5)) +
  scale_fill_manual(values = pnw_palette("Bay", 5)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  ylab("Biomass (t)") +
  xlab("Year") +
  guides(linetype = "none") +
  annotate(geom="text", x = 1984, y = 6000, label = "B[msy]", parse = TRUE)
  

