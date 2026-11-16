# Global CRW grid: 7200 x 3600 cells of 0.05 degrees.
# cell_id = iy * 7200 + ix, with ix counted eastward from -180 and iy counted
# southward from +90. Cell centres: lon = -179.975 + 0.05 * ix,
# lat = 89.975 - 0.05 * iy.

#' Cell index of the grid cell containing each coordinate
#'
#' @param lon,lat Numeric vectors in decimal degrees. Longitudes in either
#'   -180..180 or 0..360 are accepted.
#' @return Integer vector of `cell_id` values.
#' @export
crw_cell_id <- function(lon, lat) {
  stopifnot(length(lon) == length(lat), all(abs(lat) <= 90, na.rm = TRUE))
  lon <- ((lon + 180) %% 360) - 180
  ix <- pmin(floor((lon + 180) / 0.05), 7199)
  iy <- pmin(floor((90 - lat) / 0.05), 3599)
  as.integer(iy * 7200 + ix)
}

#' Cell-centre coordinates for cell indices
#'
#' @param cell_id Integer vector of cell indices.
#' @return A tibble with `cell_id`, `lon` and `lat`.
#' @export
crw_cell_lonlat <- function(cell_id) {
  tibble::tibble(
    cell_id = as.integer(cell_id),
    lon = round(-179.975 + 0.05 * (cell_id %% 7200), 3),
    lat = round(89.975 - 0.05 * (cell_id %/% 7200), 3)
  )
}
