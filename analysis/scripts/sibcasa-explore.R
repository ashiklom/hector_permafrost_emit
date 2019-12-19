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
    unit = "Gt CH4",
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

resp_data <- paste0("Resp_ann_", rcps) %>%
  map(read_excel, path = sib_file) %>%
  setNames(rcps) %>%
  bind_rows(.id = "rcp") %>%
  mutate(
    variable = "rh",
    unit = "Gt CO2",
    year = floor(`Date (yr)`)
  ) %>%
  select(
    variable, unit, rcp, year,
    CNRM = 3, GISS = 4, HARD = 5, IPSL = 6, MPI = 7
  ) %>%
  pivot_longer(c(CNRM:MPI), "model", "value")

sib_data <- bind_rows(methane_data, temp_data, resp_data)

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

plt <- ggplot(sib_wide) +
  aes(x = Tgav, y = ch4, color = rcp) +
  geom_point() +
  facet_wrap(vars(model))
plt

#' # Fitting a dynamic model
#'
#' What we actually want is to fit a model that accounts for declines in the available methane pool as emissions happen.
#' This is hard to solve for analytically, so we will instead fit it numerically.

ch4_model <- function(Tgav, CH4_0, tau, q10,
                      params = NULL) {
  if (!is.null(params)) {
    CH4_0 <- params[1]
    tau <- params[2]
    q10 <- params[3]
    alpha <- 0
  }
  ntime <- length(Tgav)
  # Constant through time
  mult <- q10 ^ (Tgav / 10)
  alt <- alpha * ifelse(Tgav > 0, log(Tgav), 0)
  # Initial conditions
  result <- matrix(NA_real_, ntime, 2)
  colnames(result) <- c("flux", "CH4")
  result[1, "flux"] <- (CH4_0 / tau) * mult[1]
  result[1, "CH4"] <- CH4_0 - result[1, "flux"] + alt[1]
  for (t in seq(2, ntime)) {
    result[t, "flux"] <- (result[t - 1, "CH4"] / tau) * mult[t]
    result[t, "CH4"] <- result[t - 1, "CH4"] - result[t, "flux"] + alt[t]
  }
  result
}
sib_wide_sub <- sib_wide %>%
  filter(rcp == "85", model == "CNRM",
         !is.na(ch4), !is.na(Tgav)) %>%
  arrange(year)
tgav <- sib_wide_sub[["Tgav"]]
obs <- sib_wide_sub[["ch4"]]
objective <- function(params) {
  pred <- ch4_model(tgav, params = params)
  sum((obs - pred[, "flux"]) ^ 2)
}
bestfit <- optim(c(20, 5e4, 1e3, 2), objective)
output <- ch4_model(tgav, params = bestfit$par)
par(mfrow = c(2, 1))
plot(obs, col = "black")
lines(output[, "flux"], type = 'l', col = "red")
plot(output[, "CH4"], type = 'l')

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
