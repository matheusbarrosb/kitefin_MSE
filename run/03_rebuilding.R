# Calculate rebuilding times ---------------------------------------------------
# This is preliminary, need to determine range of harvest rates

t.mu = rep(NA, length(h))
t.sd = rep(NA, length(h))
h = seq(0.0001, 0.025, by = 0.00002)
K = length(h)
N = 10000

for (k in 1:K) {
  
  pars = list(
    r    = c(get.par("r", fit)[2], 0.002), # could be deterministic for r and K, vary biomass only?
    Fm   = c(h[k], 0),
    Fmsy = c(get.par("Fmsyd", fit)[2], get.par("Fmsyd", fit)[4]),
    B    = c(biomass_df[51, 1], 37),
    Bmsy = c(get.par("Bmsyd", fit)[2], get.par("Bmsyd", fit)[4])
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
  ylim(c(0, 140))

ggsave("rebuilding_time.pdf", path = fig_dir)
