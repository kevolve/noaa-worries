#' Read the archive configuration
#'
#' The configuration lists candidate data roots, the NOAA server location and
#' named regions. Lookup order: the `path` argument, the `CRW_CONFIG`
#' environment variable, then `crw_config.json` in the functions folder
#' (`A_scripts/functions` under the working directory, or the folder set
#' with `options(crw.functions_dir = )`).
#'
#' @param path Optional path to a `crw_config.json` file.
#' @return A named list.
#' @export
crw_config <- function(path = NULL) {
  candidates <- c(
    path,
    Sys.getenv("CRW_CONFIG", unset = NA),
    file.path(crw_functions_dir(), "crw_config.json")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("crw_config.json not found.")
  jsonlite::fromJSON(found[1], simplifyVector = TRUE)
}

# Folder holding the R functions, crw_archive.py and crw_config.json.
crw_functions_dir <- function() {
  getOption("crw.functions_dir", file.path("A_scripts", "functions"))
}

#' Locate the data root
#'
#' Lookup order: `options(crw.root = )`, the `CRW_ROOT` environment variable,
#' then the first existing directory under `roots` in the configuration
#' (HPC path first, local path second).
#'
#' @return Path to the archive root.
#' @export
crw_root <- function() {
  root <- getOption("crw.root", Sys.getenv("CRW_ROOT", unset = ""))
  if (nzchar(root)) return(root)
  roots <- unlist(crw_config()$roots)
  hit <- roots[dir.exists(roots)]
  if (!length(hit)) {
    stop("No data root found. Set options(crw.root = ) or CRW_ROOT, or edit ",
         "'roots' in crw_config.json.")
  }
  unname(hit[1])
}
