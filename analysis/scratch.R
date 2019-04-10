library(drake)
library(magrittr)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

devtools::load_all(".")
expose_imports("hector.permafrost.emit")

rcps <- paste0("RCP", c("26", "45", "6", "85"))

mtco2_gtc <- (12 / 48) * (1 / 1000)
mtch4_gtc <- (12 / 16) * (1 / 1000)

#########################################

make(bind_plans(scenarios_plan, combined_plan))

readd(scenario_names)

loadd(hope_lo)

loadd(all_results)
loadd(all_scenarios)

all_results %>%
  dplyr::filter(
    year > 2000, year <= 2100,
    !grepl("schaefer", permafrost)
  ) %>%
  ggplot() +
  aes(x = year, y = value, color = RCP, linetype = permafrost) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  scale_linetype(drop = TRUE)

#########################################
no_permafrost <- tibble::tibble(
  Date = 2000:2100,
  exo_emissions = 0,
  exo_ch4_emissions = 0
)

run_hector_emissions("45", no_permafrost)

loadd(all_results)
loadd(gcam_results)
## loadd(tidy_scenarios)
readd(combined_results) %>%
  dplyr::count(variable)

##################################################
# Sensitivity analysis
sensitivity_analysis <- function(x, y, alpha = 0.01) {
  fit <- loess(y ~ x)
  xbar <- median(x)
  xpred <- xbar * c(1 + alpha, 1 - alpha)
  ypred <- predict(fit, xpred)
  diff(ypred) / diff(xpred)
}

temp <- dplyr::filter(lastyear, variable == "Tgav")
f <- with(temp, loess(value ~ beta + q10))
beta_bar <- with(temp, median(beta))
q10_bar <- with(temp, median(q10))
beta_pm <- beta_bar * c(0.99, 1.01)
q10_pm <- q10_bar * c(0.99, 1.01)
dbeta_in <- cbind(beta = beta_pm, q10 = rep(q10_bar, 2))
dq10_in <- cbind(beta = rep(beta_bar, 2), q10 = q10_pm)
dbeta <- diff(predict(f, dbeta_in)) / diff(beta_pm)
dq10 <- diff(predict(f, dq10_in)) / diff(q10_pm)

plot(f)
summary(f)

lastyear_wide <- lastyear %>%
  dplyr::select(-units) %>%
  tidyr::spread(variable, value)

results %>%
  dplyr::select(variable, param, param_cv, sensitivity, elasticity, partial_var)


with(results, purrr::map2(lfit, deriv_at, ~predict(.x, .y)))
results$lfit[1:5] %>% purrr::map2(~.x)

beta_fit <- loess(Tgav ~ beta, lastyear_wide)
beta_median <- median(lastyear_wide[["beta"]])
alpha <- 0.01
beta_pred <- beta_median * c(1 + alpha, 1 - alpha)
beta_sensitivity <- diff(predict(beta_fit, beta_pred)) / diff(beta_pred)

beta_spline <- with(lastyear_wide, splinefun(beta, Tgav, method = "monoH.FC"))

ggplot(lastyear) +
  aes(x = beta, y = value) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(variable), scales = "free_y")

##################################################
if (FALSE) {
  devtools::install("~/Projects/hector_project/hector")
}

library(hector)
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging = TRUE, name = "mytest")

fetchvars(core, NA, "f_nppv")
setvar(core, NA, "f_nppv", )

##################################################
library(ggplot2)

# Dirichlet distribution
means <- c(nppv = 0.35, nppd = 0.6)
palpha <- c(means, npps = 1 - sum(means))
alpha <- palpha / salpha
draws <- rdirichlet(1000, alpha)
GGally::ggpairs(as.data.frame(draws))

l <- list(id = 1, a = 5, b = 6)
l[["id"]] <- NULL

x <- hector_with_params(id = 1, beta = 0.36, q10_rh = 2)

##################################################
z <- tibble(
  x = structure(rnorm(100), unit = "cm"),
  y = runif(100)
)

attr(z$y, "unit")

##################################################
lastyear
dat <- filter(lastyear, variable == "Tgav")
fit <- mgcv::gam(value ~ beta + q10_rh + f_nppv + f_nppd + f_litterd, data = dat)
summary(fit)
pred_df <- lastyear %>% dplyr::select(q10_rh:f_litterd, -dplyr::starts_with("f_luc")) %>%
  dplyr::slice(1) %>%
  as.list() %>%
  modifyList(list(beta = c(0.03, 0.04))) %>%
  tibble::as_tibble()
pred <- predict(fit, pred_df)

##################################################
library(drake)
library(tidyverse)
dat <- readd(lastyear) %>%
  filter(variable == "Tgav")
means <- dat %>% summarize_at(vars(beta:f_litterd), mean)

fit1 <- mgcv::gam(value ~ beta + q10_rh + f_nppv + f_nppd + f_litterd, data = dat)
fit2 <- mgcv::gam(value ~ s(beta) + s(q10_rh) + s(f_nppv) + s(f_nppd) + s(f_litterd), data = dat)

pd <- as_tibble(modifyList(as.list(means), list(beta = seq(0.1, 1, 0.1))))
y <- predict(fit2, pd, se.fit = TRUE)
plot(pd$beta, y$fit)
lines(pd$beta, y$fit + y$se.fit, lty= "dashed")
lines(pd$beta, y$fit - y$se.fit, lty= "dashed")
