#' Python interpreter used for archive building
#'
#' Lookup order: `options(crw.python = )`, the `CRW_PYTHON` environment
#' variable, then `python3` on the search path.
#'
#' @return Path to a Python executable.
#' @export
crw_python <- function() {
  py <- getOption("crw.python", Sys.getenv("CRW_PYTHON", unset = ""))
  if (!nzchar(py)) py <- Sys.which("python3")
  if (!nzchar(py)) stop("python3 not found. Set options(crw.python = ) or CRW_PYTHON.")
  unname(py)
}

crw_script <- function() {
  script <- file.path(crw_functions_dir(), "crw_archive.py")
  if (!file.exists(script)) {
    stop("crw_archive.py not found at ", script,
         ". Run from the repository root or set options(crw.functions_dir = ).")
  }
  script
}

#' Run a command of the Python archive tool
#'
#' @param command One of "check", "update", "ingest", "extract", "status".
#' @param args Character vector of further command-line arguments.
#' @param root Data root; `NULL` lets the tool resolve it from `CRW_ROOT` or
#'   the configuration file.
#' @param python Python executable.
#' @param error Stop when the tool exits with a non-zero status.
#' @return The exit status, invisibly.
#' @export
crw_archive <- function(command, args = character(), root = NULL,
                        python = crw_python(), error = TRUE) {
  full <- c(shQuote(crw_script()),
            if (!is.null(root)) c("--root", shQuote(root)),
            command, args)
  status <- system2(python, full)
  if (error && status != 0) {
    stop("crw_archive.py ", command, " exited with status ", status, ".")
  }
  invisible(status)
}

#' Check that the Python environment can build the archive
#'
#' Verifies that numpy, netCDF4 and pyarrow import at usable versions and
#' that the data root is writable. Installation instructions are printed by
#' the tool when a package is missing.
#'
#' @param network Also test the connection to the NOAA server.
#' @inheritParams crw_archive
#' @return `TRUE` when all checks pass, otherwise `FALSE` (invisibly).
#' @export
crw_check_python <- function(network = FALSE, root = NULL, python = crw_python()) {
  status <- crw_archive("check", if (network) "--network", root = root,
                        python = python, error = FALSE)
  invisible(status == 0)
}

year_args <- function(years) {
  if (is.null(years)) character() else c("--years", as.character(years))
}

#' Download new NOAA files into the archive
#'
#' Lists the NOAA server, fetches files not yet archived, verifies md5
#' checksums, converts each file to parquet and records it in the manifest.
#'
#' @param product Product names, e.g. "sst", "dhw". For monthly and annual
#'   data "sst" covers sst-mean, sst-min and sst-max.
#' @param timeframe Any of "daily", "monthly", "annual", "climatology".
#' @param years Integer vector of years; `NULL` for 1985 to the present.
#' @param workers Number of parallel download/convert processes.
#' @param keep_raw Keep the NetCDF files under `raw/`.
#' @param dry_run Only report what would be fetched.
#' @inheritParams crw_archive
#' @export
crw_update <- function(product = "sst", timeframe = "daily", years = NULL,
                       workers = 4, keep_raw = FALSE, dry_run = FALSE,
                       root = NULL, python = crw_python()) {
  if (!crw_check_python(root = root, python = python)) {
    stop("Python environment check failed.")
  }
  args <- c("--product", product, "--timeframe", timeframe, year_args(years),
            "--workers", workers, if (keep_raw) "--keep-raw", if (dry_run) "--dry-run")
  crw_archive("update", args, root = root, python = python)
}

#' Build or refresh a regional subset of the archive
#'
#' Regional files hold one year each, sorted by cell then date, which makes
#' time-series reads fast. Years whose source files are unchanged are skipped.
#'
#' @param region Region name defined in the configuration (default "gbr").
#' @param bbox Optional `c(lon_min, lon_max, lat_min, lat_max)` overriding
#'   the configured bounds.
#' @inheritParams crw_update
#' @export
crw_extract_region <- function(region = "gbr", product = "sst", timeframe = "daily",
                               years = NULL, bbox = NULL, root = NULL,
                               python = crw_python()) {
  args <- c("--region", region, "--product", product, "--timeframe", timeframe,
            year_args(years), if (!is.null(bbox)) c("--bbox", bbox))
  crw_archive("extract", args, root = root, python = python)
}

#' Print a summary of the archive
#' @inheritParams crw_archive
#' @export
crw_status <- function(root = NULL, python = crw_python()) {
  crw_archive("status", root = root, python = python)
}
