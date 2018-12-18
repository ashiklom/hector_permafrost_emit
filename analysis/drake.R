library(drake)
library(magrittr)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

devtools::load_all(".")

read_schaefer <- function(file, data_type) {
  dat <- readr::read_csv(file, col_names = FALSE, col_types = "dd") %>%
    dplyr::select(year = X1, r_pc = X2)
  ymin <- ceiling(min(dat[["year"]]))
  ymax <- floor(max(dat[["year"]]))
  new_years <- seq(ymin, ymax)
  dat_interp <- spline(dat[["year"]], dat[["r_pc"]], xout = new_years)
  tibble::as_tibble(dat_interp) %>%
    dplyr::select(year = x, !!data_type := y)
}

schaefer_template <- drake_plan(
  schaefer_file = file_in("analysis/data/raw_data/schaefer_teb_rpc_ZZZ.csv"),
  schaefer = read_and_interpolate(schaefer_file_ZZZ, 2050, 2100) %>%
    # Convert cumulative emissions to annual
    dplyr::mutate(value = c(0, diff(value))) %>%
    create_scenario(cc = ., ch4_frac = 0)
)

schaefer_plan <- evaluate_plan(
  schaefer_template,
  wildcard = "ZZZ", values = c("min", "mean", "max")
)

mtco2_gtc <- (12/48) * (1 / 1000)
mtch4_gtc <- (12/16) * (1 / 1000)

hope_template <- drake_plan(
  hope_ch4_file = file_in("analysis/data/raw_data/hope_2016_MtCH4_ZZZ.csv"),
  hope_co2_file = file_in("analysis/data/raw_data/hope_2016_MtCO2_ZZZ.csv"),
  hope = create_scenario(
    co2 = read_and_interpolate(hope_co2_file_ZZZ, 2012, 2100, scale = mtco2_gtc),
    ch4 = read_and_interpolate(hope_ch4_file_ZZZ, 2012, 2100, scale = 1)
  )
)

hope_plan <- evaluate_plan(
  hope_template,
  wildcard = "ZZZ",
  values = c("lo", "mean", "hi")
)

baseline_plan <- drake_plan(
  no_permafrost_rcp45 = readr::read_csv(
    system.file("input/emissions/RCP45_emissions.csv", package = "hector"),
    skip = 3
  )
)

scenarios_plan <- bind_plans(schaefer_plan, hope_plan, baseline_plan)
combined_plan <- scenarios_plan %>%
  dplyr::filter(!grepl("_file_", target)) %>%
  gather_plan(target = "scenario_list") %>%
  dplyr::add_row(
    target = "all_scenarios",
    command = "dplyr::bind_rows(scenario_list, .id = 'scenario')"
  )

plan <- bind_plans(scenarios_plan, combined_plan)
config <- drake_config(plan)
make(plan)

########################
# An exploratory plot.
# TODO: This will eventually be merged into drake plan above

readd(all_scenarios) %>%
  dplyr::filter(
    Date > 1900,
    Date <= 2100,
    !grepl("schaefer", scenario)
  ) %>%
  dplyr::select(Date, scenario, ffi_emissions, CH4_emissions) %>%
  tidyr::gather(variable, value, -Date, -scenario) %>%
  ggplot() +
  aes(x = Date, y = value, color = scenario) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y")

ggsave("analysis/figures/permafrost_scenarios.png", width = 6, height = 5)
