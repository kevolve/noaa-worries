utils::globalVariables(c("cell_id", "date", "point"))

#' Open part of the archive as an Arrow dataset
#'
#' Nothing is read until `dplyr::collect()` is called. Value columns are
#' packed integers named `<product>_x<multiplier>` (e.g. `sst_x100`);
#' `crw_scale()` converts them after collection.
#'
#' @param product Product name, e.g. "sst", "dhw", "sst-mean".
#' @param timeframe "daily", "monthly" or "annual".
#' @param region `NULL` for the global archive, or a region name such as "gbr".
#' @param root Archive root.
#' @return An `arrow::Dataset`.
#' @export
crw_open <- function(product = "sst", timeframe = c("daily", "monthly", "annual"),
                     region = NULL, root = crw_root()) {
  timeframe <- match.arg(timeframe)
  dir <- if (is.null(region)) {
    file.path(root, timeframe, product)
  } else {
    file.path(root, "regions", region, timeframe, product)
  }
  if (!dir.exists(dir)) stop("No archive data at ", dir)
  arrow::open_dataset(dir, format = "parquet")
}

#' Convert packed integer columns to real values
#'
#' Columns named `<name>_x<multiplier>` are divided by the multiplier and
#' renamed to `<name>`.
#'
#' @param df A data frame collected from the archive.
#' @return The data frame with unpacked value columns.
#' @export
crw_scale <- function(df) {
  packed <- grep("_x[0-9]+$", names(df), value = TRUE)
  for (col in packed) {
    mult <- as.numeric(sub(".*_x([0-9]+)$", "\\1", col))
    names(df)[names(df) == col] <- sub("_x[0-9]+$", "", col)
    df[[sub("_x[0-9]+$", "", col)]] <- df[[sub("_x[0-9]+$", "", col)]] / mult
  }
  df
}

#' Time series at point coordinates
#'
#' Each point is matched to the grid cell containing it. Reading from a
#' regional subset is much faster than reading from the global archive.
#'
#' @param lon,lat Numeric vectors of point coordinates.
#' @param from,to Optional date limits (inclusive).
#' @inheritParams crw_open
#' @return A tibble with `point`, `lon`, `lat`, `cell_id`, `date` and the
#'   unpacked value column, ordered by point and date. Points on land or
#'   outside the region return no rows.
#' @export
crw_read_points <- function(lon, lat, product = "sst",
                            timeframe = c("daily", "monthly", "annual"),
                            region = NULL, from = NULL, to = NULL,
                            root = crw_root()) {
  timeframe <- match.arg(timeframe)
  pts <- tibble::tibble(point = seq_along(lon), lon = lon, lat = lat,
                        cell_id = crw_cell_id(lon, lat))
  ids <- unique(pts$cell_id)
  ds <- crw_open(product, timeframe, region, root) %>%
    dplyr::filter(cell_id %in% ids)
  if (!is.null(from)) {
    from <- as.Date(from)
    ds <- ds %>% dplyr::filter(date >= from)
  }
  if (!is.null(to)) {
    to <- as.Date(to)
    ds <- ds %>% dplyr::filter(date <= to)
  }
  values <- ds %>%
    dplyr::select(-dplyr::any_of("year")) %>%
    dplyr::collect() %>%
    crw_scale()
  pts %>%
    dplyr::inner_join(values, by = "cell_id", relationship = "many-to-many") %>%
    dplyr::arrange(point, date)
}

#' Read the climatology table
#'
#' One row per ocean cell with the maximum monthly mean (MMM) and the twelve
#' monthly mean SST climatologies, as archived by
#' `crw_update(timeframe = "climatology")`.
#'
#' @param cell_id Optional vector of cells to keep.
#' @inheritParams crw_open
#' @return A tibble of unpacked climatology values.
#' @export
crw_climatology <- function(cell_id = NULL, root = crw_root()) {
  path <- file.path(root, "climatology", "climatology.parquet")
  if (!file.exists(path)) stop("No climatology at ", path)
  ids <- cell_id
  ds <- arrow::open_dataset(path, format = "parquet")
  if (!is.null(ids)) ds <- ds %>% dplyr::filter(cell_id %in% ids)
  ds %>% dplyr::collect() %>% crw_scale()
}
