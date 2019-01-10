library(ggplot2)

import::from(magrittr, "%>%")

gcam_db_path <- "~/Projects/hector_project/permafrost_emit/gcam-output/"

conn <- rgcam::localDBConn(gcam_db_path, "database_basexdb")
project_file <- file.path(gcam_db_path, "permafrost.dat")

scenarios <- c("no_permafrost", paste0("hope_", c("lo", "mean", "hi"))) %>%
  purrr::walk(~rgcam::addScenario(conn, project_file, scenario = .x))

prj <- rgcam::loadProject(project_file)

getvar <- function(x, var) {
  dplyr::mutate(x[[var]], variable = !!var)
}

climate <- c("CO2 concentrations", "Climate forcing", "Global mean temperature") %>%
  purrr::map_dfr(~purrr::map_dfr(prj, getvar, var = .x, .id = "variable")) %>%
  dplyr::mutate(
    scenario = factor(scenario, scenarios),
    variable = forcats::fct_inorder(variable)
  ) %>%
  dplyr::filter(year > 1975)

climate %>%
  dplyr::filter(year == max(year)) %>%
  tidyr::spread(scenario, value) %>%
  dplyr::mutate(diff = hope_hi - hope_lo) %>%
  dplyr::select(variable, diff)

ggplot(climate) +
  aes(x = year, y = value, linetype = scenario) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

gdp <- prj %>%
  purrr::map_dfr("GDP by region") %>%
  dplyr::mutate(
    scenario = factor(scenarios),
    region = forcats::fct_inorder(region)
  )

gdp %>%
  dplyr::filter(scenario %in% c("hope_lo", "hope_hi")) %>%
  tidyr::spread(scenario, value) %>%
  dplyr::mutate(diff = hope_hi - hope_lo)

ggplot(dat) +
  aes(x = year, y = value, linetype = scenario) +
  geom_line()

drake::readd(all_scenarios) %>%
  dplyr::group_by(scenario) %>%
    dplyr::summarize_at(
      dplyr::vars(exo_emissions, exo_ch4_emissions),
      sum
    )

drake::readd(all_results) %>%
  dplyr::filter()

drake::readd(hope_hi) %>%
  dplyr::select(-Date) %>%
  dplyr::summarize_all(sum)

names(gcam_project)
