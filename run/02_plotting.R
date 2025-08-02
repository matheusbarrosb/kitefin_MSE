# load simulation data ---------------------------------------------------------
res_data_dir = file.path(here::here(), "res", "data", "rds/")
output_list  = readRDS(file = paste0(res_data_dir, "bycatch.rds"))

# organize output --------------------------------------------------------------
biomass_df_list = list()
catch_df_list   = list()
for (i in 1:N_BCRs) {
  biomass      = lapply(output_list[[i]], function(x) x$biomass$absolute_biomass)
  biomass_mat  = do.call(rbind, biomass)
  biomass_mean = apply(biomass_mat, 2, mean)
  biomass_sd   = apply(biomass_mat, 2, sd)
  
  catch       = lapply(output_list[[i]], function(x) x$catch[,1])
  catch_mat   = do.call(rbind, catch)
  catch_mean  = apply(catch_mat, 2, mean)
  catch_sd    = apply(catch_mat, 2, sd)
  
  biomass_df = data.frame(
    year    = seq(1972, 2020 + sim_years),
    biomass = biomass_mean,
    sd      = biomass_sd
  )
  
  catch_df = data.frame(
    year   = seq(2020, 2020 + sim_years-1),
    catch  = catch_mean,
    sd     = catch_sd
  )
  
  biomass_df_list[[i]] = biomass_df %>%
    mutate(h = rep(BCRs[i], nrow(biomass_df)))
  
  catch_df_list[[i]] = catch_df %>%
    mutate(h = rep(BCRs[i], nrow(catch_df)))
  
}

# summarize --------------------------------------------------------------------
biomass_df = do.call(rbind, biomass_df_list)

biomass_df %>%
  group_by(as.factor(h)) %>%
  summarise(
    mean_biomass = mean(biomass, na.rm = TRUE),
    sd_biomass   = sd(biomass)
  ) %>%
  arrange(mean_biomass)

# plot -------------------------------------------------------------------------

### biomass trajectories ##$
biomass_df %>%
  mutate(h = gsub("_", " ", h)) %>%
  ggplot(aes(x = year, y = biomass/1000, group = h)) +
  geom_line(aes(color = h)) +
  geom_ribbon(aes(ymin = biomass/1000 - 4*sd/1000, ymax = biomass/1000 + 4*sd/1000, fill = h), alpha = 0.4) +
  custom_theme() +
  scale_color_manual(values = pnw_palette("Bay", n = N_BCRs, type = "continuous")) +
  scale_fill_manual(values = pnw_palette("Bay", n = N_BCRs, type = "continuous")) +
  facet_wrap(~ h, ncol = 2) +
  theme(legend.position = "right") +
  geom_hline(yintercept = Bmsy/1000, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "black") +
  annotate("text", x = 2000, y = 6400/1000, label = "italic(B[msy])", parse = TRUE, color = "black") +
  theme(legend.position = "none") +
  ylab("Biomass (1000s of tonnes)") +
  xlab("Year") +
  ylim(0,12) 

ggsave("biomass_bycatch.pdf", path = fig_dir)

### mean biomass ###

# exclude observed period (1972 to 2021)
biomass_df %>%
  filter(year > 2021) %>%
  group_by(h) %>%
  mutate(h = gsub("_", " ", h)) %>%
  summarise(
    mean_biomass = mean(biomass, na.rm = TRUE),
    sd_biomass   = sd(biomass)
  ) %>%
  arrange(mean_biomass) %>%
  ggplot(aes(x = reorder(h, mean_biomass), y = mean_biomass/1000)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = (mean_biomass - 2*sd_biomass)/1000, ymax = (mean_biomass + 2*sd_biomass)/1000), width = 0.2) +
  custom_theme() +
  xlab("") +
  ylab("Mean Biomass (1000s of tonnes)") +
  coord_flip() +
  theme(legend.position = "none")

ggsave("mean_biomass_bycatch.pdf", path = fig_dir)

### catch trajectories ###
catch_df = do.call(rbind, catch_df_list)

catch_df %>%
  ggplot(aes(x = year, y = catch, group = h)) +
  geom_line(aes(color = h)) +
  geom_ribbon(aes(ymin = catch - sd, ymax = catch+ sd, fill = h), alpha = 0.4) +
  custom_theme() +
  scale_color_manual(values = pnw_palette("Bay", n = N_BCRs, type = "continuous")) +
  scale_fill_manual(values = pnw_palette("Bay", n = N_BCRs, type = "continuous")) +
  facet_wrap(~ h, ncol = 2) +
  ylim(0,250) +
  theme(legend.position = "none") +
  ylab("Catch (tonnes)") +
  xlab("Year")

ggsave("catch_bycatch.pdf", path = fig_dir)

### mean catch ###
catch_df %>%
  group_by(h) %>%
  mutate(h = gsub("_", " ", h)) %>%
  summarise(
    mean_catch = mean(catch, na.rm = TRUE),
    sd_catch   = sd(catch)
  ) %>%
  arrange(mean_catch) %>%
  ggplot(aes(x = reorder(h, mean_catch), y = mean_catch)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = (mean_catch - 2*sd_catch), ymax = (mean_catch + 2*sd_catch)), width = 0.2) +
  custom_theme() +
  xlab("") +
  ylab("Mean Catch (tonnes)") +
  coord_flip() +
  theme(legend.position = "none")

ggsave("mean_catch_bycatch.pdf", path = fig_dir)

### proportion of years fishery is closed ###
catch_df %>%
  group_by(h) %>%
  mutate(h = gsub("_", " ", h)) %>%
  summarise(
    closed_years = sum(catch == 0, na.rm = TRUE),
    total_years  = n(),
    proportion_closed = closed_years / total_years
  ) %>%
  arrange(proportion_closed) %>%
  ggplot(aes(x = reorder(h, proportion_closed), y = proportion_closed)) +
  geom_point(size = 3, shape = 21, color = "black") +
  custom_theme() +
  xlab("") +
  ylab("Proportion of Years Fishery is Closed") +
  coord_flip() +
  theme(legend.position = "none")

ggsave("proportion_closed_bycatch.pdf", path = fig_dir)
