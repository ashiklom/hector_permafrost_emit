#' ---
#' title: "Draft permafrost framework"
#' author: "Alexey Shiklomanov"
#' ---

#' # Introduction
#'
#' General framework:
#' Air temperature ($T_{air}$) --> Soil temperature ($T_{soil}$) --> Permafrost thaw area ($A_{thaw}$)
#' --> Labile permafrost C ($C_{thaw}$) --> Total C flux from permafrost ($F_{permafrost}$) --> Total CH4 flux ($CH4_{permafrost}$)
#'
#' # Setup
#'
#' Do a basic Hector run to have some global temperature data to work with.

library(hector)
library(tidyverse)
library(readxl)
library(tsibble, exclude = "id")
stopifnot(
  requireNamespace("cowplot", quietly = TRUE),
  requireNamespace("here", quietly = TRUE)
)
theme_set(cowplot::theme_cowplot())

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
hc <- newcore(rcp45, suppresslogging = TRUE)
invisible(run(hc))
tgav <- fetchvars(hc, 1800:2100, GLOBAL_TEMP(), "default") %>%
  as_tibble()

#' Load data from SibCasa runs.

sib_file <- here::here(
  "analysis", "data", "raw_data",
  "yumashev-nature-data", "Perm_simulations_SiBCASA.xlsx"
)
tidy_sib <- function(sheet) {
  models <- c("CNRM", "GISS", "HARD", "IPSL", "MPI")
  model_rxp <- sprintf("(%s)", paste(models, collapse = "|"))
  read_excel(sib_file, sheet) %>%
    mutate(year = floor(`Date (yr)`)) %>%
    select(year, matches(model_rxp)) %>%
    pivot_longer(c(-year), names_to = "model", values_to = sheet) %>%
    mutate(model = str_extract(model, model_rxp) %>% factor(models))
}

# Global temperature - Delta deg C
# Methane - Gt CH4
# Respiration - Gt CO2
# Permafrost volume - km3
# Permafrost temperature - deg C

sib_all_rcps <- map(excel_sheets(sib_file), tidy_sib) %>%
  reduce(full_join, c("year", "model"))

# TODO: For now, only worry about RCP45. Will add the others later
sib_all <- sib_all_rcps %>%
  select(year, model, matches("45$"))

sib_all_long <- sib_all %>%
  pivot_longer(c(-year, -model), names_to = "variable", values_to = "value")

ggplot(sib_all_long) +
  aes(x = year, y = value, color = model) +
  geom_line() +
  facet_grid(vars(variable), scales = "free_y")

#' # Relationship between air and permafrost temperature
#'
#' First, let's get a sense of the relationships in the data.

plt <- ggplot(sib_all) +
  aes(x = Glob_T_anom_45, y = Perm_T_45) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(model))
plt

#' For CNRM, IPSL, and MPI, the relationship is mostly linear, with some saturation around Tgav = 3.
#' For GISS and HARD, the relationship is closer to an exponential.
#' As a first approximation, let's assume the relationship is linear.

plt + geom_smooth(method = "lm", se = FALSE, color = "red")

air_perm_temp_fits <- sib_all %>%
  select(model, Glob_T_anom_45, Perm_T_45) %>%
  filter(!is.na(Perm_T_45), !is.na(Glob_T_anom_45)) %>%
  group_by(model) %>%
  nest() %>%
  mutate(lm_fit = map(data, lm, formula = Perm_T_45 ~ Glob_T_anom_45))
air_perm_temp_coefs <- air_perm_temp_fits %>%
  mutate(lm_coefs = map(lm_fit, broom::tidy)) %>%
  select(model, lm_coefs) %>%
  unnest(lm_coefs) %>%
  select(model, term, estimate) %>%
  pivot_wider(names_from = "term", values_from = "estimate")

air_perm_temp_coefs

#' # Relationship between permafrost temperature and permafrost volume
#'
#' Again, let's start with getting a sense of the relationships in the data.

plt <- ggplot(sib_all) +
  aes(x = Perm_T_45, y = Perm_vol_45) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(model), scales = "fixed")
plt

#' Again, some of the relationships are slightly curvier than others,
#' but a linear fit seems like a good first approximation.

plt + geom_smooth(method = "lm", se = FALSE, color = "red")

perm_temp_vol_fits <- sib_all %>%
  select(model, Perm_T_45, Perm_vol_45) %>%
  filter(!is.na(Perm_T_45), !is.na(Perm_vol_45)) %>%
  group_by(model) %>%
  nest() %>%
  mutate(lm_fit = map(data, lm, formula = Perm_vol_45 ~ Perm_T_45))
perm_temp_vol_coefs <- perm_temp_vol_fits %>%
  mutate(lm_coefs = map(lm_fit, broom::tidy)) %>%
  select(model, lm_coefs) %>%
  unnest(lm_coefs) %>%
  select(model, term, estimate) %>%
  pivot_wider(names_from = "term", values_from = "estimate")
perm_temp_vol_coefs

#' # Relationship between permafrost volume and permafrost labile C content
#'
#' The SiBCASA results don't provide any data on this.
#' However, it is easy enough to estimate based on global observations of permafrost C.

# Maximum permafrost volume (km3)
max_pf_vol <- max(sib_all[["Perm_vol_45"]], na.rm = TRUE)

# Pg C in surface (0-3 m deep) permafrost, from Schuur et al. 2015 [schuur_2015_climate]
surface_pf <- 1035

# Permafrost C density
pf_c_density <- surface_pf / max_pf_vol

#' A reasonable first estimate of globally-averaged permafrost C density is
#' `r pf_c_density` Pg C km$^{-3}$.
#' Let's use this number to add C density estimates to the SibCASA data.
sib_all_orig <- sib_all
sib_all <- sib_all_orig %>%
  mutate(perm_c = (max_pf_vol - Perm_vol_45) * pf_c_density)

ggplot(sib_all) +
  aes(x = year, y = perm_c, color = model) +
  geom_line()

#' # Estimating permafrost C emissions
#'
#' This is the hardest piece, because C emissions from permafrost depend on both
#' soil temperature and C availability (which is itself an indirect function soil temperature).
#' Again, let's start by getting a sense of the data.

sib_all %>%
  select(year, model, Resp_ann_45, Perm_vol_45, Perm_T_45, perm_c) %>%
  filter(!is.na(Resp_ann_45)) %>%
  group_by(model) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(cum_resp = cumsum(Resp_ann_45)) %>%
  ungroup() %>%
  mutate(actual_perm_c = perm_c - cum_resp) %>%
  pivot_longer(c(-year, -model), names_to = "variable", values_to = "value") %>%
  ggplot() +
  aes(x = year, y = value, color = model) +
  geom_line() +
  facet_grid(vars(variable), scales = "free_y")

#' Can we get away with estimating methane emissions as a fraction of overall
#' permafrost emissions?
sib_all %>%
  mutate(pre2100 = year <= 2100) %>%
  filter_at(vars(ends_with("ann_45")), all_vars(!is.na(.))) %>%
  ggplot() +
  aes(x = Resp_ann_45, y = Meth_ann_45, color = Perm_T_45) +
  geom_point() +
  facet_wrap(vars(model), scales = "fixed") +
  scale_color_viridis_c()

#' Not quite. Methane emissions start _above_ zero at the beginning, and, more
#' importantly, decline more slowly than respiration at high temperature values
#' (post 2100).
#'
#' The model we are fitting looks like this:
#'
#' \[
#' F(t) = C(t) (1 - p) (1 - \exp\frac{T_0 - T(t)}{\tau})
#' \]
#'
#' where $t$ is the current time step,
#' $F(t)$ is the total C decomposition,
#' $C(t)$ is the current total C pool,
#' $p$ is the fractional size of the passive C pool (time-invariant parameter),
#' $T_0$ is the baseline permafrost temperature,
#' $T(t)$ is the current permafrost temperature,
#' and $\tau$ is the e-folding time of permafrost decomposition.
#'
#' Total respiration

sib_sub <- sib_all %>%
  filter(
    model == "CNRM",
    !is.na(Resp_ann_45),
    !is.na(perm_c)
  ) %>%
  arrange(year)
mF <- sib_sub[["Resp_ann_45"]]
mC <- sib_sub[["perm_c"]] * 44 / 12
mT <- sib_sub[["Perm_T_45"]]
mstart <- c(p = 0.0043, T0 = -12, tau = 70)

plot(Resp_ann_45 ~ Glob_T_anom_45, data = sib_sub)
plot(mF ~ mT)

mod <- function(pars, mT, mC) {
  p <- pars[1]
  p <- 0.043
  T0 <- pars[2]
  tau <- pars[3]
  ntime <- length(mT)
  mF <- numeric(ntime)
  mC2 <- mC[1]
  for (i in seq(1, ntime)) {
    aa <- exp((T0 - mT[i]) / tau)
    mF[i] <- mC2 * (1 - p) * (1 - aa)
    mC2 <- mC[i + i] - mF
  }
  mF
}
ll <- function(x) {
  sum((mF - mod(x, mT, mC)) ^ 2)
}
mfit <- optim(mstart, ll)
result <- sib_sub %>%
  mutate(pred = perm_c * (1 - mfit$par["p"]) *
           (1 - exp((mfit$par["T0"] - Perm_T_45) / mfit$par["tau"])))
ggplot(result) +
  aes(x = year) +
  geom_line(aes(y = Resp_ann_45, color = "obs")) +
  geom_line(aes(y = pred, color = "pred"))

# mfit <- nls(mF ~ mC * (1 - p) * (1 - exp((T0 - mT) / tau)),
#             start = mstart)
mfit <- nls(Resp_ann_45 ~ perm_c * (1 - p) * (1 - exp((T0 - Perm_T_45) / tau)),
            data = sib_sub,
            start = mstart)


#' The main reason C emissions here are nonlinear is because of the confounding factor of permafrost depth:
#' The more permafrost thaws, the more warming it takes to thaw more permafrost because it occurs deeper in the soil.
#'
#' The SiBCASA results don't include anything about permafrost depth,
#' but we can derive depth by combining its permafrost volume estimates
#' with an independent estimates of permafrost area sensitivity to temperature
#' (from Chadburn et al. 2017).
#' Specifically, Chadburn et al. estimate a sensitivity of 4 million km2 deg C-1.

pf_temp_slope <- 4.0e6  # km2 degC -1

#' We can use this value to estimate the area loss from each temperature scenario,
#' and, combined with the estimates of permafrost volume, the average permafrost depth.

# Baseline for Chadburn et al. is 1960-1990
pf_area_baseline <- 15.5e6 # km2
baseline <- sib_all %>%
  filter(year >= 1960, year <= 1990) %>%
  summarize_at(vars(c(-year, -model)), mean)

sib_all2 <- sib_all %>%
  mutate(
    ## perm_area = pf_area_baseline - pf_temp_slope *
    ##   (Glob_T_anom_45 - baseline[["Glob_T_anom_45"]]), # km2
    perm_depth = (Perm_vol_45 / pf_area_baseline) * 1000 # Meters
  )

ggplot(sib_all2) +
  aes(x = year, y = perm_depth, color = model) +
  geom_line()

##################################################
#' Let's smooth out the wiggles to better approximate Hector behavior.

sib_smooth <- sib_all %>%
  as_tsibble(index = year, key = model) %>%
  mutate_at(
    vars(-c(year, model)),
    ~slide_dbl(.x, mean, .size = 25, align = "center")
  ) %>%
  filter_at(vars(-c(year, model)), all_vars(!is.na(.)))
sib_smooth_long <- sib_smooth %>%
  pivot_longer(c(-year, -model), names_to = "variable", values_to = "value")

ggplot(sib_smooth_long) +
  aes(x = year, y = value, color = model) +
  geom_line() +
  facet_grid(vars(variable), scales = "free_y")

ggplot(sib_smooth) +
  aes(x = Glob_T_anom_45, y = Meth_ann_45, color = year) +
  geom_point() +
  facet_wrap(vars(model), scales = "fixed") +
  scale_color_viridis_c()

#' Let's experiment with some functional forms.
#'
#' First, the effective temperature

plot(function(x) x * exp(-x * 0.2), to = 4)
abline(0, 1, lty = 2)

plot(function(x) 2 * (1 - exp(-x)), to = 10)
