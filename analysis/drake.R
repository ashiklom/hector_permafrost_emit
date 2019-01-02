library(drake)
library(magrittr)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

devtools::load_all(".")
expose_imports("hector.permafrost.emit")

rcps <- paste0("RCP", c("26", "45", "6", "85"))

mtco2_gtc <- (12 / 48) * (1 / 1000)
mtch4_gtc <- (12 / 16) * (1 / 1000)

baseline_template <- drake_plan(
  no_permafrost = readr::read_csv(
    system.file("input/emissions/ZZZ_emissions.csv", package = "hector"),
    skip = 3
  ) %>% dplyr::mutate(exo_emissions = 0)
)

baseline_plan <- evaluate_plan(
  baseline_template,
  wildcard = "ZZZ",
  values = rcps
)

schaefer_template <- drake_plan(
  schaefer_file = file_in("analysis/data/raw_data/schaefer_teb_rpc_ZZZ.csv"),
  schaefer = read_and_interpolate(schaefer_file_ZZZ, 2050, 2100) %>%
    # Convert cumulative emissions to annual
    dplyr::mutate(value = c(0, diff(value))) %>%
    create_scenario(
      cc = .,
      ch4_frac = 0,
      base_scenario_data = no_permafrost_YYY
    )
)

schaefer_plan <- evaluate_plan(
  schaefer_template,
  rules = list(
    ZZZ = c("min", "mean", "max"),
    YYY = rcps
  )
)

hope_template <- drake_plan(
  hope_ch4_file = file_in("analysis/data/raw_data/hope_2016_MtCH4_ZZZ.csv"),
  hope_co2_file = file_in("analysis/data/raw_data/hope_2016_MtCO2_ZZZ.csv"),
  hope = create_scenario(
    co2 = read_and_interpolate(
      hope_co2_file_ZZZ,
      2012, 2100,
      scale = mtco2_gtc
    ),
    ch4 = read_and_interpolate(
      hope_ch4_file_ZZZ,
      2012, 2100,
      scale = 1
    ),
    base_scenario_data = no_permafrost_YYY
  )
)

hope_plan <- evaluate_plan(
  hope_template,
  rules = list(
    ZZZ = c("lo", "mean", "hi"),
    YYY = rcps
  )
)

scenarios_plan <- bind_plans(baseline_plan, schaefer_plan, hope_plan)
combined_plan <- scenarios_plan %>%
  dplyr::filter(!grepl("_file_", target)) %>%
  gather_plan(target = "scenario_list") %>%
  dplyr::add_row(
    target = "all_scenarios",
    command = "dplyr::bind_rows(scenario_list, .id = 'scenario')"
  )

run_template <- drake_plan(
  results = run_hector_emissions(emissions__)
)

run_plan <- evaluate_plan(
  run_template,
  wildcard = "emissions__",
  values = scenarios_plan %>%
    dplyr::filter(!grepl("_file_", target)) %>%
    dplyr::pull(target)
)

results_plan <- gather_plan(run_plan, target = "results_list") %>%
  dplyr::add_row(
    target = "all_results",
    command = "dplyr::bind_rows(results_list, .id = 'id')"
  )

plot_plan <- drake_plan(
  tidy_scenarios = all_scenarios %>%
    tidyr::gather(variable, value, -Date, -scenario) %>%
    dplyr::rename(year = Date) %>%
    dplyr::mutate(datatype = "emissions"),
  tidy_results = all_results %>%
    tibble::as_tibble() %>%
    dplyr::select(-scenario) %>%
    dplyr::rename(scenario = id) %>%
    dplyr::mutate(
      scenario = gsub("results_", "", scenario),
      datatype = "results"
    ),
  tidy_all = dplyr::bind_rows(tidy_scenarios, tidy_results) %>%
    dplyr::mutate(
      RCP = gsub(".*(RCP.*)", "\\1", scenario) %>%
        forcats::fct_inorder(),
      permafrost = gsub("_RCP.*", "", scenario) %>%
        gsub("results_", "", .) %>%
        forcats::fct_inorder()
    ),
  all_figure = tidy_all %>%
    dplyr::filter(
      year > 2000, year <= 2100,
      !grepl("schaefer", permafrost),
      !(datatype == "emissions" & !grepl("ffi|CH4|exo", variable)),
      ) %>%
    dplyr::mutate(
      variable = forcats::fct_inorder(variable)
    ) %>%
    ggplot() +
    aes(x = year, y = value, color = RCP, linetype = permafrost) +
    geom_line() +
    facet_wrap(variable ~ ., scales = "free_y") +
    scale_color_viridis_d(),
  all_figure_png = ggsave(
    file_out("analysis/figures/all_results.png"),
    all_figure,
    width = 8, height = 6, units = "in"
  )
)

plan <- bind_plans(scenarios_plan, combined_plan, run_plan,
                   results_plan, plot_plan)
config <- drake_config(plan)
make(plan)
