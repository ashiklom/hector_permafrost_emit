library(hector)
library(hectortools)
library(tidyverse)
library(BayesianTools)

GMT_REF <- 13 # Hector reference temperature, degrees C

hector_dir <- "~/Projects/hector_project/hector"
cmip5_dir <- file.path(
  hector_dir,
  "scripts",
  "outputplotter",
  "comparison_data"
)
cmip5_files <- list.files(cmip5_dir, recursive = TRUE, full.names = TRUE)

model_file <- file.path(cmip5_dir, "GMD_2014", "IPCC", "IPCC2005.csv")
model_file <- file.path(cmip5_dir, "GMD_2015", "CMIP5",
                        "Hector_models_tas", "rcp45", "ensemble_mean_model.csv")

model_data <- read_csv(model_file)

hc <- newcore(system.file("input", "hector_rcp45.ini", package = "hector"),
              name = "hector",
              suppresslogging = TRUE)

run(hc)
result <- fetchvars(hc, 2000:2100, "Tgav")

model_data %>%
  filter(
    year <= 2100,
    vtag == "tgav",
    value > 13
  ) %>%
  ggplot() +
  aes(x = year, y = value - min(value), color = model) +
  geom_line() +
  geom_line(aes(color = "hector"), data = result, linetype = "dashed")

model_wide <- model_data %>%
  filter(year >= 2006, year <= 2100, vtag == "tgav") %>%
  select(model, year, value) %>%
  spread(model, value)

model_tgav_abs <- model_wide %>%
  select(-year) %>%
  as.matrix()

## model_tgav <- sweep(model_tgav_abs, 2, apply(model_tgav_abs, 2, min))
model_tgav <- sweep(model_tgav_abs, 2, GMT_REF)

# Define test likelihood
run_hector_params <- function(params, pb = NULL) {
  if (!is.null(pb)) pb$tick()
  beta <- params[1]
  q10 <- params[2]
  reset(hc)
  setvar(hc, NA, BETA(), beta, NA)
  setvar(hc, NA, Q10_RH(), q10, NA)
  run(hc, 2100)
  result <- fetchvars(hc, 2006:2100, "Tgav")
  result
}

hc <- newcore(system.file("input", "hector_rcp45.ini", package = "hector"),
              name = "hector",
              suppresslogging = TRUE)

matplot(model_tgav, type = "l")
lines(run_hector_params(c(0.36, 2.45))[["value"]], col = "black", lwd = 2)
## lines(run_hector_params(c(0.0, 30))[["value"]], col = "black", lwd = 2)
## lines(run_hector_params(c(20, 2.45))[["value"]], col = "black", lwd = 2)

loglike <- function(params) {
  hector <- tryCatch(
    run_hector_params(params),
    error = function(e) {
      message("Hit the following error running Hector: ", conditionMessage(e))
      FALSE
    })
  if (isFALSE(hector)) return(-1e15)
  hector_tgav <- hector[["value"]]
  diff <- model_tgav - hector_tgav
  ll <- sum(dnorm(diff, 0, sd = 0.01, log = TRUE))
  ll
}

hc <- newcore(system.file("input", "hector_rcp45.ini", package = "hector"),
              name = "hector",
              suppresslogging = TRUE)
prior <- createUniformPrior(lower = c(0, 1), upper = c(3, 1000), best = c(0.36, 2.45))
setup <- createBayesianSetup(loglike, prior, names = c("beta", "q10"))
samples <- runMCMC(setup, settings = list(iterations = 2000))
samples <- runMCMC(samples, settings = list(iterations = 2000))

gelmanDiagnostics(samples)
tracePlot(samples, start = 500)


params_df <- expand.grid(beta = seq(0, 1, 0.05), q10 = seq(1, 3, 0.1)) %>%
  as_tibble()

sims_df <- params_df %>%
  mutate(results = map2(beta, q10, ~run_hector_params(c(.x, .y))))

sims <- sims_df %>%
  mutate(id = row_number()) %>%
  unnest(results)

ggplot(sims) +
  aes(x = year, y = value, group = id) +
  geom_line(alpha = 0.2, size = 0.5)

