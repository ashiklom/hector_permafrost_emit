#' ---
#' title: "Kessler permafrost framework"
#' author: "Alexey Shiklomanov"
#' ---

library(tidyverse)
library(hector)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
hc <- newcore(rcp45)
invisible(run(hc))
tgav_df <- fetchvars(hc, 1800:2100, GLOBAL_TEMP()) %>%
  as_tibble()

year0 <- 2000

tgav <- tgav_df %>%
  filter(variable == GLOBAL_TEMP(),
         year > year0) %>%
  arrange(year) %>%
  pull(value)

tgav_pf <- tgav - tgav[1]

#' Kessler estimate
beta <- 0.172

#' Simple linear model (can consider alternatives)
frac_thawed <- pmin(1.0, tgav_pf * beta)

#' Based on Hugelius et al. 2014, Schuur et al. 2015 (Kessler, Table 2)
#' (Pg C or Gt C)
permafrost_c_total <- 1035

c_thawed <- permafrost_c_total * frac_thawed

#' Kessler decomposition framework:
#'
#' \[
#' C_{\text{decomp}}(t) = C_{\text{thawed}}(t) \times \left( 1 - p_{\text{passive}} \right) \times \left( 1 - \exp \frac{t - t_0}{\tau} \right)
#' \]

#' Passive C fraction ($\p{\text{passive}}$), based on Kessler
prop_passive <- 0.4

#' E-folding time of permafrost.
tau <- 70

delta_time <- seq_along(c_thawed) - 1
pf_years <- year0 + delta_time

c_decomp <- c_thawed * (1 - prop_passive) * (1 - exp(-delta_time / tau))
plot(c_decomp ~ pf_years, type = "l",
     xlab = "Year",
     ylab = expression("Total C emissions" ~ (PgC ~ year ^ -1)))

#' Proportion of methane emissions, from Kessler
prop_ch4 <- 0.023

ch4_decomp <- c_decomp * prop_ch4
plot(ch4_decomp ~ pf_years, type = "l",
     xlab = "Year",
     ylab = expression(CH[4] ~ "emissions" ~ (PgC ~ year ^ -1)))

#' A more dynamic model would accommodate the fact that the thawed C pool is
#' shrinking as it is respired.

ntime <- length(tgav_pf)
flux <- numeric(ntime)
c_decomp2 <- numeric(ntime)
flux_prev <- 0
for (t in seq_len(ntime)) {
  flux[t] <- (c_thawed[t] - flux_prev) *
    (1 - prop_passive) *
    (1 - exp(-t / tau))
  flux[t] <- max(0, flux[t])
  flux_prev <- flux[t]
}

plot(c_decomp ~ pf_years, type = "l")
lines(pf_years, flux, col = 2)
