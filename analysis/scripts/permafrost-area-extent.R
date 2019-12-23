library(raster)
library(fs)
library(magrittr, exclude = "extract")

ncdir <- here::here(
  "analysis", "data", "raw_data",
  "chadburn_estimated_permafrost_maps"
)

# Download data if it doesn't exist
if (!dir_exists(ncdir)) {
  ncdir_zip <- paste0(ncdir, ".zip")
  if (!file_exists(ncdir_zip)) {
    message("Downloading Chadburn permafrost maps")
    download.file(
      "http://store.pangaea.de/Publications/Chadburn-etal_2017/estimated_permafrost_maps_NetCDF.zip",
      ncdir_zip
    )
  }
  message("Extracting Chadburn permafrost maps")
  unzip(ncdir_zip)
} else {
  message("Chadburn permafrost files already downloaded")
}

stereo_nh <- paste("+proj=stere +lat_0=90 +lon_0=-98 +k=0.9996",
                   "+x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs")


degree <- 1
get_area <- function(degree) {
  filename <- path(ncdir, sprintf("map_%smean.nc", as.character(degree)))
  r <- raster(filename)
  # Northern Hemisphere
  nh <- extent(-180, 180, 0, 90)
  r_nh <- crop(r, nh)
  r_nh_proj <- projectRaster(r_nh, crs = stereo_nh)
  vals <- getValues(r_nh_proj)
  vals[vals < 0] <- 0
  sum(vals, na.rm = TRUE)
}

areas <- tibble::tibble(
  degree = c(1, 1.5, 2, 3, 4, 5, 6),
  perm_area = purrr::map_dbl(
    degree,
    purrr::possibly(get_area, NA_real_)
  )
)

areas2 <- areas %>%
  dplyr::add_row(degree = 0, perm_area = get_area("historical_")) %>%
  dplyr::arrange(degree)

plot(perm_area ~ degree, data = areas, type = 'o',
     ylab = expression("Permafrost area" ~ (m^2)),
     xlab = expression(Delta ~ "temperature" ~ (degree * C)))

temp_area_spline <- splinefun(
  areas[["degree"]],
  areas[["perm_area"]]
)

saveRDS(
  temp_area_spline,
  here::here("analysis", "data", "derived-data",
             "temperature-area-spline.rds")
)
