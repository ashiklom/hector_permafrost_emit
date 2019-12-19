#' ---
#' title: "Estimating the temperature-CH4 relationship from SiBCASA outputs"
#' author: Alexey Shiklomanov
#' output_format:
#'   rmarkdown::html_document
#' ---

#' # Setup
#'
#' Load necessary libraries

library(readxl)
library(tidyverse)
library(tsibble, exclude = "id")
library(cowplot)
theme_set(theme_cowplot())

#' SiBCASA simulations,
#' from [Yumashev et al. 2019 Nature](https://doi.org/10.1038/s41467-019-09863-x),
#' supplementary data 4.
#' Those data include SiBCASA simulations of methane emissions and global temperature (prescribed by scenarios?)
#' from RCP 4.5 and 8.5.
#' The results also include estimates of permafrost area extent, but we'll exclude those for now.

sib_file <- here::here(
  "analysis", "data", "raw_data",
  "yumashev-nature-data", "Perm_simulations_SiBCASA.xlsx"
)

rcps <- c("45", "85")

methane_data <- paste0("Meth_ann_", rcps) %>%
  map(read_excel, path = sib_file) %>%
  setNames(rcps) %>%
  bind_rows(.id = "rcp") %>%
  mutate(
    variable = "ch4",
    unit = "Gt",
    year = floor(`Date (yr)`)
  ) %>%
  select(
    variable, unit, rcp, year,
    CNRM = 3, GISS = 4, HARD = 5, IPSL = 6, MPI = 7
  ) %>%
  pivot_longer(c(CNRM:MPI), "model", "value")

temp_data <- paste0("Glob_T_anom_", rcps) %>%
  map(read_excel, path = sib_file) %>%
  setNames(rcps) %>%
  bind_rows(.id = "rcp") %>%
  mutate(
    variable = "Tgav",
    unit = "K",
    year = floor(`Date (yr)`)
  ) %>%
  select(
    variable, unit, rcp, year,
    CNRM = 3, GISS = 4, HARD = 5, IPSL = 6, MPI = 7
  ) %>%
  pivot_longer(c(CNRM:MPI), "model", "value")

sib_data <- bind_rows(methane_data, temp_data)

#' A graphical summary of the results:

ggplot(sib_data) +
  aes(x = year, y = value, color = model) +
  geom_line() +
  facet_grid(vars(variable), vars(rcp), scales = "free_y")

#' # Estimating the temperature-CH4 relationship

#' We can look at the general relationship between methane emissions and temperature over time.

sib_wide <- sib_data %>%
  mutate(year = floor(year)) %>%
  select(-unit) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  as_tsibble(index = year, key = c(rcp, model))

ggplot(subset(sib_wide, Tgav <= 4)) +
  aes(x = Tgav, y = ch4, color = rcp) +
  geom_point() +
  facet_wrap(vars(model))

#' For a dynamic emissions model like the one we want to include in Hector,
#' what we _really_ want is the relationship between changes in temperature
#' and changes in methane emissions.

sib_wide %>%
  group_by_key() %>%
  mutate(dch4 = ch4 - lag(ch4), dTgav = Tgav - lag(Tgav)) %>%
  ggplot() +
  aes(x = dTgav, y = dch4, color = rcp) +
  geom_point() +
  facet_wrap(vars(model))

#' This version has basically no relationship because of the many interannual wiggles in the simulations.
#' Below, we smooth those out with a 20 year running mean to produce a stronger signal.

window_size <- 20
sib_wide_d <- sib_wide %>%
  group_by_key() %>%
  mutate(ch4_slide = slide_dbl(ch4, mean, na.rm = TRUE, .size = window_size),
         Tgav_slide = slide_dbl(Tgav, mean, na.rm = TRUE, .size = window_size),
         dch4_slide = ch4_slide - lag(ch4_slide),
         dTgav_slide = Tgav_slide - lag(Tgav_slide)) %>%
  ungroup()

ggplot(sib_wide_d) +
  aes(x = dTgav_slide, y = dch4_slide) +
  geom_point() +
  facet_wrap(vars(model))

#' Large increases in temperature have a disproportionately large impact on methane emissions,
#' so an exponential regression ($y = a e^{bx}$, or $log(y) = \alpha + \beta x$) is a good choice.
#' This also agrees well with the typical mathematical representation of temperature-dependent reactions.
#'
#' Below, we fit an exponential regression by model:

sib_wide_fits_n <- sib_wide_d %>%
  as_tibble() %>%
  select(model, dTgav_slide, dch4_slide) %>%
  group_by(model) %>%
  nest() %>%
  mutate(
    fit = map(data, lm, formula = log(dch4_slide) ~ dTgav_slide),
    pred = map2(data, fit, modelr::add_predictions),
    resid = map2(data, fit, modelr::add_residuals)
  ) %>%
  ungroup()

#' The resulting fits:

ggplot() +
  aes(x = dTgav_slide, y = dch4_slide) +
  geom_point(data = unnest(sib_wide_fits_n, data),
             color = "gray60") +
  geom_line(aes(y = exp(pred)), color = "blue", size = 1,
            data = unnest(sib_wide_fits_n, pred)) +
  facet_wrap(vars(model))

#' And the resulting coefficients
#' (note that the "intercept" is really $e ^ Intercept$, so it is a value very close to zero):

sib_fit_coefs <- sib_wide_fits_n %>%
  mutate(coef_df = map(fit, broom::tidy)) %>%
  select(model, coef_df) %>%
  unnest(coef_df)
sib_fit_coefs

#' # Offline (decoupled) simulation using Hector
#'
#' Now, let's try to plug this into a Hector simulation.
#' The model itself is not yet implemented dynamically in Hector,
#' but as a first pass, we can try to neglect the feedback from CH4 back onto temperature.
#' We can do this by just applying this temperature increase to Hector's predictions of temperature.
#'
#' First, we setup the Hector core.

library(hector)
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
hc <- newcore(rcp45, suppresslogging = TRUE)

#' Then, we create a separate permafrost biome.
#' We will assume that roughly 25% of the world's soil carbon, and 10% of its vegetation, are in permafrost regions.

split_biome(hc, "global", c("default", "permafrost"),
            fveg_c = c(0.9, 0.1),
            fsoil_c = c(0.75, 0.25))

invisible(run(hc))
results <- fetchvars(hc, 1800:2100, c(GLOBAL_TEMP(), ATMOSPHERIC_CH4())) %>%
  as_tibble()
hector_temp <- results %>%
  filter(variable == GLOBAL_TEMP())

#' Now, let's apply the coefficients from the SiBCASA to the temperature results.
#' First, we write a function that will produce results for each model.

sib_coefs_wide <- sib_fit_coefs %>%
  select(model, term, estimate) %>%
  pivot_wider(names_from = "term", values_from = "estimate")

sib_dch4 <- function(Tgav) {
  log_dch4 <- sib_coefs_wide[["(Intercept)"]] +
    sib_coefs_wide[["dTgav_slide"]] * Tgav
  dch4 <- exp(log_dch4)
  names(dch4) <- sib_coefs_wide[["model"]]
  dch4
}

hector_ch4 <- hector_temp %>%

#' As a _very_ rough first pass, let's take the mean of all these coefficients to use as default values in Hector.

sib_fit_means <- sib_fit_coefs %>%
  group_by(term) %>%
  summarize(value = mean(estimate)) %>%
  deframe() %>%
  setNames(c("intercept", "slope"))
sib_fit_means

#' #
