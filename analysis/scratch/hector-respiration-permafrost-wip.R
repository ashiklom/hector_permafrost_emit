#' ---
#' title: "Permafrost based on Hector's respiration model"
#' author: Alexey Shiklomanov
#' ---

library(tidyverse)
library(hector)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
hc <- newcore(rcp45)
invisible(run(hc))
years <- seq(1750, 2100)
out_vars <- c(GLOBAL_TEMP(), SOIL_C())
hector_df <- fetchvars(hc, years, out_vars) %>%
  as_tibble() %>%
  arrange(year)

tgav <- hector_df %>%
  filter(variable == GLOBAL_TEMP()) %>%
  pull(value)

soil_temperature <- function(tair, nwindow = 200) {
  tair_ext <- c(rep(0, nwindow), tair)
  tsoil <- as.numeric(stats::filter(
    tair_ext,
    rep(1 / nwindow, nwindow),
    sides = 1
  ))
  tsoil[seq(nwindow + 1, length(tsoil))]
}

tsoil <- soil_temperature(tgav)
matplot(years, cbind(tgav, tsoil), type = "l",
        lty = "solid", col = c(1, 2),
        xlab = "Year",
        ylab = expression(Temperature ~ (degree * C)))
legend("topleft", c("Air", "Soil"), lty = "solid",
       col = c(1, 2))

#' Hector respiration functions
rh_detritus <- function(detritus_c, temp_atm, q10_rh) {
  tempfertd <- q10_rh ^ (temp_atm / 10)
  detritus_c * 0.25 * tempfertd
}

rh_soil <- function(soil_c, temp_soil, q10_rh) {
  tempferts <- q10_rh ^ (temp_soil / 10)
  soil_c * 0.02 * tempferts
}

soil_c <- hector_df %>%
  filter(variable == SOIL_C()) %>%
  pull(value)
