# Heat stress metrics following the NOAA Coral Reef Watch 5 km v3.1
# methodology (https://coralreefwatch.noaa.gov/product/5km/methodology.php;
# Liu et al. 2014, Remote Sensing 6:11579-11606; Skirving et al. 2020,
# Remote Sensing 12:3856).
#
#   HotSpot:  HS_i  = SST_i - MMM, with negative values set to 0
#   DHW:      DHW_i = sum over the 84 days ending on day i of HS_j / 7,
#                     counting only days with HS_j >= 1 degree C
#
# MMM is the maximum monthly mean SST climatology of the cell. DHW is in
# degree C-weeks.

#' Coral bleaching HotSpot
#'
#' @param sst Numeric vector of SST (degrees C).
#' @param mmm Maximum monthly mean climatology (degrees C); a single value or
#'   a vector the same length as `sst`.
#' @return Numeric vector of HotSpot values (degrees C, never negative).
#' @export
crw_hotspot <- function(sst, mmm) {
  pmax(sst - mmm, 0)
}

#' Degree heating weeks from a daily SST series
#'
#' `sst` must be a gap-free daily series for one cell in date order. Days
#' with missing SST contribute no heat stress.
#'
#' @inheritParams crw_hotspot
#' @param date Optional vector of dates used to verify that the series is
#'   daily and consecutive.
#' @param window Accumulation window in days (84 = 12 weeks).
#' @param threshold Minimum HotSpot that accumulates (1 degree C).
#' @param partial If `FALSE` (default), the first `window - 1` days, whose
#'   window is incomplete, are returned as `NA`.
#' @return Numeric vector of DHW (degree C-weeks).
#' @export
crw_dhw <- function(sst, mmm, date = NULL, window = 84L, threshold = 1,
                    partial = FALSE) {
  if (!is.null(date) && length(date) > 1 && any(diff(as.Date(date)) != 1)) {
    stop("'date' must be consecutive daily dates in increasing order.")
  }
  hs <- crw_hotspot(sst, mmm)
  stress <- ifelse(!is.na(hs) & hs >= threshold, hs, 0)
  total <- cumsum(stress)
  n <- length(total)
  lagged <- c(rep(0, min(window, n)), total[seq_len(max(n - window, 0))])
  dhw <- (total - lagged) / 7
  if (!partial) dhw[seq_len(min(window - 1L, n))] <- NA_real_
  dhw
}
