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
library(patchwork)
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

ggplot(sib_all) +
  aes(x = Glob_T_anom_45, y = Perm_T_45) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(model))

#' For CNRM, IPSL, and MPI, the relationship is mostly linear, with some saturation around Tgav = 3.
#' For GISS and HARD, the relationship is closer to an exponential.
#' As a first approximation, let's assume the relationship is linear.

ggplot(sib_all) +
  aes(x = Glob_T_anom_45, y = Perm_T_45) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(vars(model))

air_perm_temp_fits <- sib_all %>%
  select(model, kj, Perm_T_45) %>%
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

ggplot(sib_all) +
  aes(x = Perm_T_45, y = Perm_vol_45) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(model), scales = "fixed")

#' Again, some of the relationships are slightly curvier than others,
#' but a linear fit seems like a good first approximation.

ggplot(sib_all) +
  aes(x = Perm_T_45, y = Perm_vol_45) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(vars(model), scales = "fixed")

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
max_pf_vol <- max(sib_vol[["Perm_vol_45"]])

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
