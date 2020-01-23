devtools::load_all("~/Projects/hector_project/hector-permafrost-submodel")
inidir <- file.path("~", "Projects", "hector_project",
                    "hector-permafrost-submodel", "inst", "input")
rcp <- "45"
inifile <- normalizePath(file.path(inidir, paste0("hector_rcp", rcp, ".ini")))
stopifnot(file.exists(inifile))
hc <- newcore(inifile)
yearz <- 2100
run(hc, yearz)
dates <- seq(1750, yearz)
outvars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), PERMAFROST_C(), SOIL_C())
no_pf <- fetchvars(hc, dates, outvars, scenario = "Original")
reset(hc)
setvar(hc, 0, PERMAFROST_C(), 1035, "PgC")
run(hc, yearz)
yes_pf <- fetchvars(hc, dates, outvars, scenario = "Permafrost")
r26 <- rbind(no_pf, yes_pf)

library(ggplot2)
ggplot(r26) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")
