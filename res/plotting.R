require(ggplot2)
require(dplyr)
require(tidyr)
require(PNWColors)
require(here)
require(ggpubr)

# load data
data_directory = file.path(here::here(), "res", "data", "rds")
rds_files      = list.files(path = data_directory, pattern = "\\.rds$", full.names = TRUE)
data_list      = lapply(rds_files, readRDS)

clean_names = sub("^\\d+[_-]*", "", tools::file_path_sans_ext(basename(rds_files)))

names(data_list) = clean_names

# 1. Biomass trajectory --------------------------------------------------------
bmsy_value = 5542.576768
n_years = 124
# wrangle data
for (k in 1:length(data_list)) {
  data_list[[k]] = Filter(function(x) !is.null(x$biomass), data_list[[k]])
}

nofishing_bmat = matrix(NA, nrow = n_years, ncol = length(data_list[[1]]))
for (j in 1:length(data_list[[1]])) {
  for (i in 1:n_years) nofishing_bmat[i, j] = data_list[[1]][[j]]$biomass[i,2]
}
year  = 1:n_years + 1980 
mu    = rowMeans(nofishing_bmat, na.rm = TRUE)
sigma = apply(nofishing_bmat, 1, sd, na.rm = TRUE)

nofishing_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "No fishing")

bycatch_bmat = matrix(NA, nrow = n_years, ncol = length(data_list[[2]]))
for (j in 1:length(data_list[[2]])) {
  for (i in 1:n_years) bycatch_bmat[i, j] = data_list[[2]][[j]]$biomass[i,2]
}
mu    = rowMeans(bycatch_bmat, na.rm = TRUE)
sigma = apply(bycatch_bmat, 1, sd, na.rm = TRUE)

bycatch_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Bycatch")

twostep_bmat = matrix(NA, nrow = n_years, ncol = length(data_list[[3]]))
for (j in 1:length(data_list[[3]])) {
  for (i in 1:n_years) twostep_bmat[i, j] = data_list[[3]][[j]]$biomass[i,2]
}
year = 1:n_years + 1980
mu    = rowMeans(twostep_bmat, na.rm = TRUE)
sigma = apply(twostep_bmat, 1, sd, na.rm = TRUE)

twostep_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Two-step HCR")

precaut_twostep_bmat = matrix(NA, nrow = n_years, ncol = length(data_list[[4]]))
for (j in 1:length(data_list[[4]])) {
  for (i in 1:n_years) precaut_twostep_bmat[i, j] = data_list[[4]][[j]]$biomass[i,2]
}
mu    = rowMeans(precaut_twostep_bmat, na.rm = TRUE)
sigma = apply(precaut_twostep_bmat, 1, sd, na.rm = TRUE)

precaut_twostep_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Precautionary two-step HCR")

hyper_precaut_twostep_bmat = matrix(NA, nrow = n_years, ncol = length(data_list[[5]]))
for (j in 1:length(data_list[[5]])) {
  for (i in 1:n_years) hyper_precaut_twostep_bmat[i, j] = data_list[[5]][[j]]$biomass[i,2]
}
mu    = rowMeans(hyper_precaut_twostep_bmat, na.rm = TRUE)
sigma = apply(hyper_precaut_twostep_bmat, 1, sd, na.rm = TRUE)

hyper_precaut_twostep_df = data.frame(year, mu, sigma) %>% mutate(Scenario = "Hyper-precautionary two-step HCR")


bio_df = rbind(nofishing_df, bycatch_df, twostep_df,
               precaut_twostep_df, hyper_precaut_twostep_df)

bio_df$phase = ifelse(bio_df$sigma == 0, "Observed", "Simulated")

left_plot <- bio_df %>%
  ggplot(aes(x = year)) +
  # Relative biomass (left axis, colored by Scenario)
  geom_line(aes(y = mu / Bmsy, color = Scenario, linetype = phase), linewidth = 1.2) +
  geom_ribbon(aes(
    ymin = (mu - sigma) / Bmsy,
    ymax = (mu + sigma) / Bmsy,
    fill = Scenario,
    group = Scenario
  ), alpha = 0.15) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  custom_theme() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.y.right = element_blank(),   # Remove right y axis text
    axis.ticks.y.right = element_blank(),   # Remove right y axis ticks
    plot.margin = margin(1, 1, 1, 1)
  ) +
  scale_color_manual(values = pnw_palette("Bay", 5)) +
  scale_fill_manual(values = pnw_palette("Bay", 5)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  guides(linetype = "none") +
  xlab(NULL) +
  annotate(geom = "text", x = 1984, y = 1.1, label = "B[msy]", parse = TRUE) +
  geom_line(aes(y = mu / bmsy_value, color = Scenario, linetype = phase), linewidth = 1.2) +
  geom_ribbon(aes(
    ymin = (mu - sigma) / bmsy_value,
    ymax = (mu + sigma) / bmsy_value,
    fill = Scenario,
    group = Scenario
  ), alpha = 0.15) +
  scale_y_continuous(
    name = "",
    limits = c(0, 2.1),
    sec.axis = sec_axis(~ . * bmsy_value, name = "")
  )

right_plot <- bio_df %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mu / Bmsy, color = Scenario, linetype = phase), linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  custom_theme() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.y.left = element_blank(),    
    axis.ticks.y.left = element_blank(),    
    plot.margin = margin(1, 1, 1, 1)
  ) +
  scale_color_manual(values = pnw_palette("Bay", 5)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  guides(linetype = "none") +
  annotate(geom = "text", x = 1984, y = 1.1, label = "B[msy]", parse = TRUE) +
  xlab(NULL) +
  geom_line(aes(y = mu / bmsy_value, color = Scenario, linetype = phase), linewidth = 1.2) +
  scale_y_continuous(
    name = "",
    limits = c(0, 2.1),
    sec.axis = sec_axis(~ . * bmsy_value, name = "")
  )

# Arrange and annotate the figure
biomass_plot <- ggarrange(
  left_plot,
  right_plot,
  common.legend = TRUE,
  legend = "top",
  align = "hv",
  widths = c(1, 1),
  heights = c(1, 1),
  ncol = 2
) %>%
annotate_figure(
  biomass_plot,
  left = text_grob("Relative biomass", rot = 90, vjust = 1, size = 14),
  right = text_grob("Absolute biomass (t)", rot = -90, vjust = 1, size = 14),
  bottom = text_grob("Year", hjust = 0.5, size = 14)
)


ggsave(
  filename = "biomass_plot.pdf",
  plot = biomass_plot,
  path = file.path(here::here(), "res", "figures"),
  width = 8,
  height = 3
)


# 2. Catch trajectory ----------------------------------------------------------

K = length(data_list)
J = length(data_list[[2]])
N = dim(data_list[[2]][[1]]$catch)

cat_list = list()
for (j in 1:J)  cat_list[[j]] = data_list[[2]][[j]]$catch
cat_mat = matrix(NA, nrow = N, ncol = J)
for (j in 1:J) {
  for (i in 1:N) cat_mat[i, j] = cat_list[[j]][i, 1]
}
bycatch_cat = data.frame(
  year = 1:N + 2020,
  mu   = rowMeans(cat_mat, na.rm = TRUE),
  sigma = apply(cat_mat, 1, sd, na.rm = TRUE)
)


K = length(data_list)
J = length(data_list[[3]])
N = dim(data_list[[3]][[3]]$catch)

cat_list = list()
for (j in 1:J)  cat_list[[j]] = data_list[[3]][[j]]$catch
cat_mat = matrix(NA, nrow = N, ncol = J)
for (j in 1:J) {
  for (i in 1:N) cat_mat[i, j] = cat_list[[j]][i, 1]
}

twostep_cat = data.frame(
  year = 1:N + 2020,
  mu   = rowMeans(cat_mat, na.rm = TRUE),
  sigma = apply(cat_mat, 1, sd, na.rm = TRUE)
) 

K = length(data_list)
J = length(data_list[[4]])
N = dim(data_list[[4]][[4]]$catch)

cat_list = list()
for (j in 1:J)  cat_list[[j]] = data_list[[4]][[j]]$catch
cat_mat = matrix(NA, nrow = N, ncol = J)
for (j in 1:J) {
  for (i in 1:N) cat_mat[i, j] = cat_list[[j]][i, 1]
}

precaut_twostep_cat = data.frame(
  year = 1:N + 2020,
  mu   = rowMeans(cat_mat, na.rm = TRUE),
  sigma = apply(cat_mat, 1, sd, na.rm = TRUE)
)

K = length(data_list)
J = length(data_list[[5]])
N = dim(data_list[[5]][[5]]$catch)
cat_list = list()
for (j in 1:J)  cat_list[[j]] = data_list[[5]][[j]]$catch
cat_mat = matrix(NA, nrow = N, ncol = J)
for (j in 1:J) {
  for (i in 1:N) cat_mat[i, j] = cat_list[[j]][i, 1]
}
hyper_precaut_twostep_cat = data.frame(
  year = 1:N + 2020,
  mu   = rowMeans(cat_mat, na.rm = TRUE),
  sigma = apply(cat_mat, 1, sd, na.rm = TRUE)
)

catch_df = rbind(
  bycatch_cat %>% mutate(Scenario = "Bycatch"),
  twostep_cat %>% mutate(Scenario = "Two-step HCR"),
  precaut_twostep_cat %>% mutate(Scenario = "Precautionary two-step HCR"),
  hyper_precaut_twostep_cat %>% mutate(Scenario = "Hyper-precautionary two-step HCR")
)

 catch_df %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mu, color = Scenario), linewidth = 1.2) +
  geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma, fill = Scenario), alpha = 0.15) +
 # custom_theme() +
  theme(
    legend.position = "none",
    plot.margin = margin(5,5,5,5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank()
  ) +
  annotate(geom = "text", x = 2027, y = 185, label = "MSY", parse = TRUE, size = 2.8) +
  geom_hline(yintercept = 151.83, linetype = "dashed", linewidth = 0.7) +
  scale_color_manual(values = pnw_palette("Bay", 4)) +
  scale_fill_manual(values = pnw_palette("Bay", 4)) +
  xlab("Year") +
  ylab("Catch (t)") +
  guides(fill = "none") +
  facet_wrap(~ Scenario, ncol = 2)
 
 
 ggsave(
   filename = "catch_ts.pdf",
   plot = last_plot(),
   path = file.path(here::here(), "res", "figures"),
   width = 8,
   height = 3
 )

# mean catch plot (means + sd)

 catch_df %>%
   group_by(Scenario) %>%
   summarise(
     mean_catch = mean(mu, na.rm = TRUE),
     sd_catch = sd(mu, na.rm = TRUE)
   ) %>%
   ggplot(aes(x = Scenario, y = mean_catch)) +
   geom_bar(stat = "identity", fill = "steelblue", color = "black") +
   geom_point() +
   geom_errorbar(aes(ymin = mean_catch - sd_catch, ymax = mean_catch + sd_catch), width = 0.2) +
   custom_theme() +
   theme(
     legend.position = "none",
     plot.margin = margin(5, 10, 10, 10)
   ) +
   xlab("") +
   ylab("Catch (t)") +
   coord_flip()

 ggsave(
   filename = "mean_catch.pdf",
   plot = last_plot(),
   path = file.path(here::here(), "res", "figures"),
   width = 6,
   height = 3
 )   



