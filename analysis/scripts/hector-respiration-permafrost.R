#' ---
#' title: "Permafrost based on Hector's respiration model"
#' author: Alexey Shiklomanov
#' ---

library(tidyverse)
library(hector)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
hc <- newcore(rcp45)
invisible(run(hc))
tgav_df <- fetchvars(hc, 1750:2100, GLOBAL_TEMP()) %>%
  as_tibble()

tgav <- tgav_df %>%
  filter(variable == GLOBAL_TEMP()) %>%
  arrange(year) %>%
  pull(value)

soil_temperature <- function(tair, nwindow = 200) {
  n <- length(tair)
  tsoil <- rep(0, n)
  if (n > nwindow) {
    tsoil <- as.numeric(stats::filter(
      # Buffer with zeros
      c(rep(0, nwindow), tair),
      rep(1 / nwindow, nwindow),
      sides = 1
    ))
    tsoil[seq(1, nwindow)] <- 0
    tsoil <- tsoil[seq(1, n)]
  }
  tsoil
}

#' Hector respiration functions
rh_detritus <- function(detritus_c, temp_atm, q10_rh) {
  tempfertd <- q10_rh ^ (temp_atm / 10)
  detritus_c * 0.25 * tempfertd
}

rh_soil <- function(soil_c, temp_soil, q10_rh) {
  temp_soil <- s
  tempferts <- q10_rh
}

#' Linear increase in temperature.
tsoil <- soil_temperature(tgav)
matplot(cbind(tgav, tsoil), type = "l")
