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
