library(drake)
library(magrittr)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")
devtools::load_all(".")
expose_imports("hector.permafrost.emit")

rcps <- paste0("RCP", c("26", "45", "6", "85"))

gcam_root <- getOption("gcam_root")
stopifnot(!is.null(gcam_root), file.exists(gcam_root))
gcam_climate <- file.path(gcam_root, "input", "climate")
gcam_exe <- file.path(gcam_root, "exe")

mtco2_gtc <- (12 / 48) * (1 / 1000)
mtch4_gtc <- (12 / 16) * (1 / 1000)

schaefer_template <- drake_plan(
  schaefer_file = file_in("analysis/data/raw_data/schaefer_teb_rpc_ZZZ.csv"),
  schaefer = read_and_interpolate(schaefer_file_ZZZ, 2050, 2100) %>%
    # Convert cumulative emissions to annual
    dplyr::mutate(
      value = c(0, diff(value)),
      exo_ch4_emissions = 0
    ) %>%
    dplyr::select(
      Date = year,
      exo_emissions = value,
      exo_ch4_emissions
    )
)

schaefer_plan <- evaluate_plan(
  schaefer_template,
  rules = list(ZZZ = c("min", "mean", "max"))
)

hope_template <- drake_plan(
  hope_ch4_file = file_in("analysis/data/raw_data/hope_2016_MtCH4_ZZZ.csv"),
  hope_co2_file = file_in("analysis/data/raw_data/hope_2016_MtCO2_ZZZ.csv"),
  hope = dplyr::full_join(
    read_and_interpolate(
      hope_co2_file_ZZZ,
      2012, 2100,
      scale = mtco2_gtc
    ),
    read_and_interpolate(
      hope_ch4_file_ZZZ,
      2012, 2100,
      scale = 1
    ),
    by = "year"
  ) %>% dplyr::select(
    Date = year,
    exo_emissions = value.x,
    exo_ch4_emissions = value.y
  )
)

hope_plan <- evaluate_plan(
  hope_template,
  rules = list(ZZZ = c("lo", "mean", "hi"))
)

scenarios_plan <- bind_plans(schaefer_plan, hope_plan)
combined_plan <- scenarios_plan %>%
  dplyr::filter(!grepl("_file_", target)) %>%
  gather_plan(target = "scenario_list") %>%
  bind_plans(drake_plan(
    scenario_names = names(scenario_list),
    all_scenarios = dplyr::bind_rows(scenario_list, .id = 'scenario')
  ))

run_template <- drake_plan(
  results = run_hector_emissions(
    rcp__,
    emissions__,
    name = paste0("emissions__", ".", "RCPrcp__")
  )
)

run_plan <- evaluate_plan(
  run_template,
  list(
    emissions__ = c("NULL", readd(scenario_names)),
    rcp__ = c("26", "45", "60", "85")
  )
)

results_plan <- bind_plans(
  gather_plan(run_plan, target = "results_list"),
  drake_plan(
    all_results = results_list %>%
      dplyr::bind_rows(.id = "id") %>%
      tibble::as_tibble() %>%
      tidyr::separate(scenario, c("permafrost", "RCP"), sep = "\\.") %>%
      dplyr::mutate(
        permafrost = forcats::fct_inorder(permafrost) %>%
          forcats::fct_recode("baseline" = "NULL"),
        RCP = forcats::fct_inorder(RCP)
      )
  )
)

gcam_plan <- drake_plan(
  csv = readr::write_csv(ZZZ, file_out("CLIM/ZZZ.csv")),
  ini = make_ini(
    file_in("CLIM/ZZZ.csv"),
    file_in("CLIM/hector-gcam.ini"),
    file_out("CLIM/ZZZ.ini")
  ),
  xml = make_xmls(
    ini_ZZZ,
    file_in("GCAM/input/gcamdata/xml/hector.xml"),
    file_in("EXE/configuration_ref.xml"),
    file_out("GCAM/input/gcamdata/xml/hector_ZZZ.xml"),
    file_out("EXE/config_ZZZ.xml")
  )
) %>%
  evaluate_plan(rules = list(ZZZ = readd(scenario_names))) %>%
  evaluate_plan(
    rules = list(GCAM = gcam_root, EXE = gcam_exe, CLIM = gcam_climate),
    rename = FALSE
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

plan <- bind_plans(scenarios_plan, combined_plan, run_plan, results_plan, gcam_plan)
                   ## plot_plan)
config <- drake_config(plan)
make(plan)
