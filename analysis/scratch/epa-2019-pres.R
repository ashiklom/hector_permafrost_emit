library(tidyverse)
library(drake, exclude = c("expand", "gather"))
library(fs)
library(cowplot)

figdir <- here::here("analysis", "figures", "epa-2019-presentation") %>%
  dir_create()

### Parameter distributions
loadd(both_densities)
param_labels <- labeller(param = as_labeller(c(
  beta = "beta[CO[2]]",
  q10_rh = "Q[10,RH]",
  f_litterd = "f(litter %->% Det.)",
  f_nppv = "f(NPP %->% Veg.)",
  f_nppd = "f(NPP %->% Det.)",
  permafrost.warmingfactor = "delta * T[Arctic]",
  fveg_c = "f(Veg. ~ C[Arctic])",
  fsoil_c = "f(Soil ~ C[Arctic])",
  fdetritus_c = "f(Det. ~ C[Arctic])"
), default = label_parsed))
densities_sub <- both_densities %>%
  mutate(
    param = forcats::fct_inorder(param)
  ) %>%
  group_by(param) %>%
  mutate(y = y / max(y)) %>%
  ungroup() %>%
  filter(!(param == "f_litterd" & x < 0.95))

ggplot(densities_sub) +
  aes(x = x, y = y) +
  geom_line() +
  lims(y = c(0, 1)) +
  labs(x = "Value", y = "Normalized probability density") +
  facet_wrap(vars(param), scales = "free_x", labeller = param_labels,
             ncol = 2) +
  cowplot::theme_cowplot() +
  theme(
    strip.background = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 8)
  )
ggsave(path(figdir, "param-distributions.png"),
       width = 4.68, height = 7)


### Time series results
loadd(sims_summary)

both_sims_2100 <- bind_rows(
  global = readd(lastyear_global_sims),
  biome = readd(lastyear_biome_sims),
  .id = "simulation_type"
)

both_sims_sub <- both_sims_2100 %>%
  filter(variable %in% c("Ca", "Tgav"))

sims_sub <- sims_summary %>%
  filter(variable %in% c("Ca", "Tgav"))

lblr <- labeller(
  variable = as_labeller(c(
    "Ca" = "Atm. ~ bgroup('[', CO[2], ']') ~ (ppm)",
    "Tgav" = "Global ~ Delta * T ~ (degree * C)"
  ), default = label_parsed)
)
set_simtype <- function(simtype) {
  simtype_labs <- c("Default", "Separate Arctic biome")
  factor(simtype, c("global", "biome"), simtype_labs)
}
ggplot(sims_sub) +
  aes(x = year, color = set_simtype(simulation_type)) +
  geom_line(aes(y = Mean)) +
  geom_ribbon(aes(ymin = lo2, ymax = hi2, fill = set_simtype(simulation_type)),
              alpha = 0.2, color = NA) +
  geom_violin(aes(x = 2110, y = value, fill = set_simtype(simulation_type)),
              data = both_sims_sub, width = 10) +
  facet_grid(vars(variable), scales = "free_y",
             switch = "y", labeller = lblr) +
  theme_cowplot() +
  theme(
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = c(0.1, 0.95),
    legend.title = element_blank()
  )
ggsave(path(figdir, "time-series-compare.png"), width = 6.62, height = 7)

### Sensitivity analysis
scatter_grid <- function(dat) {
  params <- setdiff(colnames(dat), c("id", "scenario", "year",
                                     "variable", "value", "units")) %>%
    rlang::syms()
  baseplot <- ggplot(dat) +
    aes(y = value) +
    facet_wrap(vars(variable), scales = "free_y") +
    ## geom_point() +
    geom_hex() +
    geom_smooth(method = "gam", color = "black") +
    ylab("") +
    theme_bw() +
    guides(fill = FALSE) +
    scale_fill_gradient(low = "grey80", high = "red4")
  plot_list <- lapply(params, function(X) baseplot + aes(x = !!X))
  do.call(plot_grid, plot_list)
}

loadd(lastyear_global_sims)
loadd(lastyear_biome_sims)

biome_long <- lastyear_biome_sims %>%
  filter(variable == "Tgav") %>%
  select(-c(scenario:variable, units)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-c(id, value), values_to = "param_value")

densities_sub %>%
  filter(name %in% c("beta", "q10_rh"))

lblr <- labeller(name = as_labeller(c(
  default.beta = "beta[Temp.]",
  default.q10_rh = "Q[10, Temp]",
  default.f_nppv = "f(NPP %->% Veg.)[Temp.]",
  permafrost.beta = "beta[Arc.]",
  permafrost.q10_rh = "Q[10, Arc.]",
  permafrost.f_nppv = "f(NPP %->% Veg.)[Arc.]"
), default = label_parsed))
biome_long %>%
  filter(name %in% c("default.beta", "default.q10_rh", "default.f_nppv",
                     "permafrost.beta", "permafrost.q10_rh", "permafrost.f_nppv")) %>%
  ggplot() +
  aes(x = param_value, y = value) +
  geom_hex() +
  geom_smooth(method = "gam", color = "black") +
  facet_wrap(vars(name), scales = "free_x", labeller = lblr,
             nrow = 3, dir = "v") +
  scale_fill_gradient(low = "gray90", high = "red4", trans = "log10") +
  guides(fill = FALSE) +
  labs(y = expression("Global" ~ Delta * "T" ~ "at 2100" ~ (degree * "C")),
       x = "Parameter value") +
  theme_cowplot()
ggsave(path(figdir, "parameter-sensitivity-scatter.png"),
       width = 5.31, height = 7.17)

biome_sensitivity <- readd(sensitivity_wide_sensitivity_biome_params_lastyear_biome_sims)

param_labs <- c(
  default.beta = "beta[Temp.]",
  default.q10_rh = "Q[10, Temp]",
  default.f_nppv = "f(NPP %->% Veg.)[Temp.]",
  default.f_nppd = "f(NPP %->% Veg.)[Temp.]",
  default.f_litterd = "f(Litter %->% Det.)[Temp.]",
  permafrost.beta = "beta[Arc.]",
  permafrost.q10_rh = "Q[10, Arc]",
  permafrost.f_nppv = "f(NPP %->% Veg.)[Arc.]",
  permafrost.f_nppd = "f(NPP %->% Veg.)[Arc.]",
  permafrost.f_litterd = "f(Litter %->% Det.)[Arc.]",
  permafrost.warmingfactor = "delta * T[Arctic]",
  fveg_c = "f(Veg. ~ C[Arctic])",
  fsoil_c = "f(Soil ~ C[Arctic])",
  fdetritus_c = "f(Det. ~ C[Arctic])"
)
param_labs <- c(
  default.beta = "Beta[Temp]",
  default.q10_rh = "Q10[Temp]",
  default.f_nppv = "f(NPP -> Veg)[Temp]",
  default.f_nppd = "f(NPP -> Det.)[Temp.]",
  default.f_litterd = "f(Litter -> Det.)[Temp.]",
  permafrost.beta = "Beta[Arc.]",
  permafrost.q10_rh = "Q10[Arc]",
  permafrost.f_nppv = "f(NPP -> Veg.)[Arc.]",
  permafrost.f_nppd = "f(NPP -> Veg.)[Arc.]",
  permafrost.f_litterd = "f(Litter -> Det.)[Arc.]",
  permafrost.warmingfactor = "dT[Arctic]",
  fveg_c = "f(Veg. C)[Arctic]",
  fsoil_c = "f(Soil C)[Arctic]",
  fdetritus_c = "f(Det. C)[Arctic]"
)
biome_sensitivity %>%
  filter(stat == "partial_var", variable == "Tgav") %>%
  mutate(param = factor(param, names(param_labs), param_labs) %>%
           fct_reorder(value)) %>%
  ggplot() +
  aes(x = value, y = param) +
  geom_segment(aes(xend = 0, yend = param)) +
  geom_point() +
  theme_cowplot() +
  labs(x = "Frac. variance in Global Temp. explained") +
  theme(
    axis.title.y = element_blank()
  )
