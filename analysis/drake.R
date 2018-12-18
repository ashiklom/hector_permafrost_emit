library(drake)
library(magrittr)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

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

plan <- drake_plan(
  schaefer_raw_min_file = file_in("analysis/data/raw_data/schaefer_teb_rpc_min.csv"),
  schaefer_raw_max_file = file_in("analysis/data/raw_data/schaefer_teb_rpc_max.csv"),
  schaefer_raw_mean_file = file_in("analysis/data/raw_data/schaefer_teb_rpc_mean.csv"),
  schaefer_dat = read_schaefer(schaefer_raw_min_file, "r_pc_min") %>%
    dplyr::full_join(read_schaefer(schaefer_raw_max_file, "r_pc_max")) %>%
    dplyr::full_join(read_schaefer(schaefer_raw_mean_file, "r_pc_mean")) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      r_pc_mean = dplyr::case_when(
        year < 2025 ~ 0,
        TRUE ~ r_pc_mean
      ),
      r_pc_min = dplyr::case_when(
        year <= 2060 ~ r_pc_mean,
        TRUE ~ r_pc_min
      ),
      r_pc_max = dplyr::case_when(
        year <= 2060 ~ r_pc_mean,
        TRUE ~ r_pc_max
      )
    ),
  hector_rcp45 = readr::read_csv(
    system.file("input/emissions/RCP45_emissions.csv", package = "hector"),
    skip = 3
  )
)

config <- drake_config(plan)
make(plan)

loadd(schaefer_dat)
readd(hector_rcp45)

rcp45 <- readr::read_csv("~/Projects/hector_project/hector/inst/input/emissions/RCP45_emissions.csv", skip = 3)

ggplot(schaefer_dat %>% dplyr::filter(year < 2100)) +
  aes(x = year, y = r_pc_mean, ymin = r_pc_min, ymax = r_pc_max) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_line()
