#### Download NOAA CoralWatch 5km NetCDF data ####
#### (c) Kevin Novak, AIMS
#### Updated 7 May 2025
####
#### Downloads .nc files from the NOAA CRW 5km v3.1 FTP server, preserving
#### the original filenames. Each .nc is accompanied by a .md5 sidecar which
#### is verified after download. Existing files whose checksums pass are
#### skipped; those that fail are re-downloaded automatically.

# ===========================================================================
# DEPENDENCIES
# ===========================================================================

pkgs <- c("tidyverse", "purrr", "RCurl", "curl", "fs")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}))


# ===========================================================================
# HELPERS
# ===========================================================================

#' Ensure a directory exists, creating it recursively if needed.
#' Returns the path with a trailing slash.
.ensure_dir <- function(path) {
  dir <- if (stringr::str_detect(path, "\\.[^/]+$")) dirname(path) else path
  if (!stringr::str_ends(dir, "/")) dir <- paste0(dir, "/")
  if (!fs::dir_exists(dir)) {
    fs::dir_create(dir, recurse = TRUE)
    cat(sprintf("  \U0001F4C1 Created: %s\n", dir))
  }
  dir
}

#' List filenames in an FTP directory.
#' Returns character(0) with a warning on access failure.
.ftp_ls <- function(url) {
  tryCatch(
    RCurl::getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE) %>%
      stringr::str_split("\n", simplify = TRUE) %>%
      as.character() %>%
      purrr::keep(stringr::str_detect(., "\\S")) %>%
      sort(),
    error = function(e) {
      warning(sprintf(
        "\u26A0\uFE0F  Cannot list FTP directory (path may not exist):\n  %s\n  %s",
        url, conditionMessage(e)
      ))
      character(0)
    }
  )
}

#' Estimate total download size by sampling up to `n` files via FTP.
#' Returns a list with mean_file_bytes, total_est_bytes, n_sampled.
.estimate_size <- function(urls, n = 10L) {
  sample_urls <- utils::head(urls, n)
  sizes <- purrr::map_dbl(sample_urls, function(u) {
    tryCatch({
      h <- curl::new_handle()
      curl::handle_setopt(h, nobody = TRUE, ftp_use_epsv = FALSE)
      resp <- curl::curl_fetch_memory(u, handle = h)
      hdr  <- rawToChar(resp$headers)
      m    <- regmatches(hdr, regexpr("(?i)Content-Length:\\s*(\\d+)", hdr, perl = TRUE))
      if (!length(m)) return(NA_real_)
      as.numeric(sub("(?i)Content-Length:\\s*", "", m, perl = TRUE))
    }, error = function(e) NA_real_)
  })
  if (all(is.na(sizes)))
    return(list(mean_file_bytes = NA_real_, total_est_bytes = NA_real_,
                n_sampled = length(sample_urls)))
  mean_sz <- mean(sizes, na.rm = TRUE)
  list(mean_file_bytes = mean_sz,
       total_est_bytes = mean_sz * length(urls),
       n_sampled       = length(sample_urls))
}

#' Check that `save_dir` has enough free space for `required_bytes` × `mult`.
.check_disk <- function(save_dir, required_bytes, mult = 1.1) {
  free <- tryCatch(as.numeric(fs::fs_usage(save_dir)$avail), error = function(e) NA_real_)
  if (is.na(free)) {
    warning("\u26A0\uFE0F  Could not read free disk space at '", save_dir, "'. Proceeding anyway.")
    return(invisible(NULL))
  }
  needed <- required_bytes * mult
  cat(sprintf("  \u2139\uFE0F  Free: %.2f GB | Estimated needed: %.2f GB\n",
              free / 1e9, needed / 1e9))
  if (free < needed)
    stop(sprintf(
      "\U0001F6AB Insufficient disk space.\n  Free: %.2f GB\n  Needed (\u00d7%.1f): %.2f GB",
      free / 1e9, mult, needed / 1e9
    ))
  invisible(NULL)
}

#' Verify a local .nc file against its .md5 sidecar.
#'
#' Three possible outcomes:
#'   "ok"       — hashes match (or no sidecar exists to compare against)
#'   "mismatch" — sidecar present but hashes differ
#'   "no_sidecar" — sidecar does not exist locally
.verify_checksum <- function(nc_path) {
  md5_path <- paste0(nc_path, ".md5")
  if (!file.exists(md5_path)) return("no_sidecar")
  
  noaa_hash  <- system(paste("cat",    shQuote(md5_path)), intern = TRUE) %>%
    stringr::str_extract("^[0-9a-fA-F]+")
  local_hash <- system(paste("md5 -q", shQuote(nc_path)),  intern = TRUE) %>%
    stringr::str_extract("^[0-9a-fA-F]+")
  
  if (is.na(noaa_hash) || is.na(local_hash) || noaa_hash != local_hash) {
    "mismatch"
  } else {
    "ok"
  }
}

#' Build the exact-match filename regex for a given measure.
#'
#' Daily SST uses the "CoralTemp" prefix (coraltemp_v3.1_YYYYMMDD.nc);
#' everything else uses ct5km_<measure>[_-].
.measure_pattern <- function(m, is_daily = FALSE) {
  if (m == "sst" && is_daily) "^coraltemp_"
  else paste0("^ct5km_", stringr::str_replace_all(m, "-", "[-_]"), "[_-]")
}


# ===========================================================================
# MAIN FUNCTION
# ===========================================================================

#' Download NOAA CRW 5km NetCDF files, preserving original filenames.
#'
#' Files are saved to subdirectories mirroring the FTP structure:
#'   <output_path>/climatology/
#'   <output_path>/<measure>/annual/
#'   <output_path>/<measure>/monthly/
#'   <output_path>/<measure>/daily/
#'
#' Each .nc file is accompanied by its .md5 checksum sidecar. On every run:
#'   1. Files already on disk are checksum-verified.
#'   2. Files that pass are skipped.
#'   3. Files that fail (mismatch) or are missing are (re-)downloaded.
#'   4. Downloaded files are checksum-verified; failures are flagged and
#'      the corrupt file + sidecar are removed so the next run retries them.
#'
#' @param output_path     Root folder for saved .nc files.
#' @param years           Integer vector of years (1985–present). Not needed
#'                        for timeframe = "climatology".
#' @param measure         One of: "sst", "dhw", "ssta", "baa", "baa-max-7d",
#'                        "baa5", "baa5-max-7d", "hs", "sst-trend-7d",
#'                        "year-to-date", "climatology".
#' @param summary_type    For annual/monthly: "mean", "min", "max", or "all"
#'                        (default). Ignored for daily and climatology.
#' @param timeframe       One or more of "daily", "monthly", "annual",
#'                        "climatology".
#' @param max_retries     Download retry attempts for stubborn files (default 5).
#' @param safety_multiplier  Disk-space headroom multiplier (default 1.1).
#'
#' @return Invisibly returns a tibble with one row per .nc file and columns:
#'   filename, local_path, ftp_url, status
#'   ["ok" | "skipped" | "downloaded" | "checksum_fail" | "download_fail"]
download_noaa_nc <- function(
    output_path       = "/Users/uqkbairo/MODRRAP/noaa-worries",
    years             = NULL,
    measure           = c("sst", "dhw", "climatology"),
    summary_type      = NULL,
    timeframe         = c("daily", "monthly", "annual", "climatology"),
    max_retries       = 5L,
    safety_multiplier = 1.1
) {
  
  # ── argument validation ────────────────────────────────────────────────────
  measure   <- match.arg(measure[1],
                         c("climatology", "baa", "baa-max-7d", "baa5", "baa5-max-7d",
                           "dhw", "hs", "sst", "sst-trend-7d", "ssta", "year-to-date"))
  timeframe <- match.arg(timeframe,
                         c("annual", "monthly", "daily", "climatology"), several.ok = TRUE)
  
  if (is.null(summary_type) || identical(summary_type, "all")) {
    summary_type <- "all"
  } else {
    bad <- setdiff(summary_type, c("mean", "min", "max"))
    if (length(bad))
      stop("Invalid summary_type(s): ", paste(bad, collapse = ", "),
           ". Use 'all', 'mean', 'min', or 'max'.")
  }
  
  daily_ok   <- c("baa","baa-max-7d","baa5","baa5-max-7d","dhw","hs",
                  "sst","sst-trend-7d","ssta","year-to-date")
  monthly_ok <- c("baa","baa5","dhw","hs","sst","ssta")
  annual_ok  <- c("baa","baa5","dhw","hs","sst","ssta")
  
  if ("daily"   %in% timeframe && !measure %in% c(daily_ok,   "climatology"))
    stop("'", measure, "' is not available at daily resolution.")
  if ("monthly" %in% timeframe && !measure %in% c(monthly_ok, "climatology"))
    stop("'", measure, "' is not available at monthly resolution.")
  if ("annual"  %in% timeframe && !measure %in% c(annual_ok,  "climatology"))
    stop("'", measure, "' is not available at annual resolution.")
  
  if (measure == "climatology") {
    timeframe <- "climatology"
    message("\u2139\uFE0F  measure = 'climatology' \u2192 timeframe forced to 'climatology'.")
  }
  
  if (any(timeframe != "climatology")) {
    if (is.null(years))
      stop("'years' must be provided for timeframe(s): ",
           paste(setdiff(timeframe, "climatology"), collapse = ", "))
    bad_yrs <- years[!years %in% 1985:2025]
    if (length(bad_yrs))
      stop("Year(s) out of range [1985\u20132025]: ", paste(bad_yrs, collapse = ", "))
  }
  
  # ── FTP base + local root ──────────────────────────────────────────────────
  base_url    <- "ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/"
  output_path <- .ensure_dir(output_path)
  
  # ── build file manifest (FTP url <-> local path pairs) ────────────────────
  cat("\U0001F50D Querying NOAA FTP server...\n")
  
  ftp_urls   <- character(0)
  local_paths <- character(0)
  
  register <- function(folder_url, filenames, local_dir) {
    ftp_urls    <<- c(ftp_urls,    paste0(folder_url, filenames))
    local_paths <<- c(local_paths, paste0(local_dir,  filenames))
  }
  
  # -- climatology --
  if ("climatology" %in% timeframe) {
    folder_url <- paste0(base_url, "climatology/")
    local_dir  <- .ensure_dir(paste0(output_path, "climatology/"))
    fnames <- .ftp_ls(folder_url) %>% stringr::str_subset("\\.nc$")
    if (length(fnames)) register(folder_url, fnames, local_dir)
  }
  
  # -- annual --
  if ("annual" %in% timeframe) {
    cat("  Fetching annual listing...\n")
    folder_url <- paste0(base_url, "nc/v1.0/annual/")
    local_dir  <- .ensure_dir(paste0(output_path, measure, "/annual/"))
    fnames <- .ftp_ls(folder_url) %>%
      stringr::str_subset("\\.nc$") %>%
      stringr::str_subset(.measure_pattern(measure))
    
    if (summary_type != "all")
      fnames <- fnames %>%
      stringr::str_subset(paste0("-", summary_type, "[_.]"))
    
    # Keep only requested years (4-digit token in filename)
    yr_pat <- paste(years, collapse = "|")
    fnames <- fnames %>%
      stringr::str_subset(paste0("(?:^|[_.])(", yr_pat, ")(?:[_.]|$)"))
    
    if (!length(fnames))
      warning(sprintf(
        "\u26A0\uFE0F  No annual files found: measure='%s', summary_type='%s', years=%s",
        measure, summary_type, paste(range(years), collapse = "\u2013")
      ))
    else
      register(folder_url, fnames, local_dir)
  }
  
  # -- monthly --
  # FTP structure: nc/v1.0/monthly/<year>/  (no measure subfolder)
  if ("monthly" %in% timeframe) {
    local_dir <- .ensure_dir(paste0(output_path, measure, "/monthly/"))
    for (yr in years) {
      cat(sprintf("  Fetching monthly listing for %d...\n", yr))
      folder_url <- paste0(base_url, "nc/v1.0/monthly/", yr, "/")
      fnames <- .ftp_ls(folder_url) %>%
        stringr::str_subset("\\.nc$") %>%
        stringr::str_subset(.measure_pattern(measure))
      
      if (summary_type != "all")
        fnames <- fnames %>%
        stringr::str_subset(paste0("^ct5km_", measure, "-", summary_type, "_"))
      
      if (length(fnames))
        register(folder_url, fnames, local_dir)
      else
        warning(sprintf(
          "\u26A0\uFE0F  No monthly files found: measure='%s', year=%d.", measure, yr
        ))
    }
  }
  
  # -- daily --
  # FTP structure: nc/v1.0/daily/<measure>/<year>/
  if ("daily" %in% timeframe) {
    local_dir <- .ensure_dir(paste0(output_path, measure, "/daily/"))
    for (yr in years) {
      cat(sprintf("  Fetching daily listing for %d...\n", yr))
      folder_url <- paste0(base_url, "nc/v1.0/daily/", measure, "/", yr, "/")
      fnames <- .ftp_ls(folder_url) %>%
        stringr::str_subset("\\.nc$") %>%
        stringr::str_subset(.measure_pattern(measure, is_daily = TRUE))
      
      if (length(fnames))
        register(folder_url, fnames, local_dir)
      else
        warning(sprintf(
          "\u26A0\uFE0F  No daily files found: measure='%s', year=%d.", measure, yr
        ))
    }
  }
  
  n_total <- length(ftp_urls)
  if (n_total == 0) {
    message("No files matched the requested parameters. Nothing to download.")
    return(invisible(tibble::tibble(
      filename = character(), local_path = character(),
      ftp_url  = character(), status = character()
    )))
  }
  cat(sprintf("\U0001F4CB %d file(s) in manifest.\n\n", n_total))
  
  # ── step 1: verify existing files ─────────────────────────────────────────
  cat("\U0001F50F Checking existing files...\n")
  
  cs_status <- purrr::map_chr(local_paths, function(p) {
    if (!file.exists(p)) return("missing")
    .verify_checksum(p)
  })
  
  n_ok      <- sum(cs_status == "ok")
  n_missing <- sum(cs_status == "missing")
  n_noside  <- sum(cs_status == "no_sidecar")
  n_bad     <- sum(cs_status == "mismatch")
  
  cat(sprintf(
    "  \u2705 OK (verified):    %d\n  \u2753 OK (no sidecar):  %d\n  \u23EC Missing:          %d\n  \u274C Mismatch:         %d\n\n",
    n_ok, n_noside, n_missing, n_bad
  ))
  
  # Remove corrupt files so they are re-downloaded cleanly
  if (n_bad > 0) {
    bad_paths <- local_paths[cs_status == "mismatch"]
    cat(sprintf("  \U0001F5D1\uFE0F  Removing %d corrupt file(s) for re-download...\n", n_bad))
    purrr::walk(bad_paths, function(p) {
      if (file.exists(p))              file.remove(p)
      if (file.exists(paste0(p, ".md5"))) file.remove(paste0(p, ".md5"))
    })
    cs_status[cs_status == "mismatch"] <- "missing"
  }
  
  # ── step 2: identify files to download ────────────────────────────────────
  # "ok" and "no_sidecar" are both treated as already-good; only download
  # (or re-download) files that are missing.
  need_dl    <- cs_status == "missing"
  dl_urls    <- ftp_urls[need_dl]
  dl_paths   <- local_paths[need_dl]
  md5_urls   <- paste0(dl_urls,  ".md5")
  md5_paths  <- paste0(dl_paths, ".md5")
  
  if (!any(need_dl)) {
    cat("\u2705 All files already present and verified. Nothing to download.\n")
    return(invisible(tibble::tibble(
      filename   = basename(local_paths),
      local_path = local_paths,
      ftp_url    = ftp_urls,
      status     = dplyr::if_else(cs_status == "ok", "ok", "skipped_no_sidecar")
    )))
  }
  
  cat(sprintf("\U0001F4E5 %d file(s) to download.\n", sum(need_dl)))
  
  # ── step 3: disk space pre-flight ─────────────────────────────────────────
  cat("\U0001F4BE Estimating download size...\n")
  sz <- .estimate_size(dl_urls)
  if (!is.na(sz$total_est_bytes)) {
    cat(sprintf("  \u2248 Mean file size: %.1f MB | Total estimate: %.2f GB\n",
                sz$mean_file_bytes / 1e6, sz$total_est_bytes / 1e9))
    .check_disk(output_path, sz$total_est_bytes, safety_multiplier)
  } else {
    warning("\u26A0\uFE0F  Could not estimate file sizes; skipping disk space check.")
  }
  
  # ── step 4: download with retries ─────────────────────────────────────────
  cat(sprintf("\n\U0001F4E5 Downloading %d .nc + .md5 file(s)...\n", sum(need_dl)))
  closeAllConnections()
  
  # Download .nc and .md5 files together
  all_dl_urls  <- c(dl_urls,  md5_urls)
  all_dl_paths <- c(dl_paths, md5_paths)
  
  remaining_urls  <- all_dl_urls
  remaining_paths <- all_dl_paths
  n_retries       <- 0L
  
  while (length(remaining_urls) > 0 && n_retries <= max_retries) {
    curl::multi_download(
      urls      = remaining_urls,
      destfiles = remaining_paths,
      resume    = TRUE
    )
    done          <- file.exists(remaining_paths)
    remaining_urls  <- remaining_urls[!done]
    remaining_paths <- remaining_paths[!done]
    
    if (!length(remaining_urls)) break
    
    n_retries <- n_retries + 1L
    cat(sprintf("  \u21BB %d file(s) not yet complete; retry %d/%d...\n",
                length(remaining_urls), n_retries, max_retries))
    Sys.sleep(10)
  }
  
  if (length(remaining_urls)) {
    # Strip .md5 URLs to report only .nc failures
    failed_nc <- remaining_urls[!stringr::str_ends(remaining_urls, "\\.md5$")]
    if (length(failed_nc))
      warning(sprintf(
        "\u274C %d .nc file(s) could not be downloaded after %d retries:\n%s",
        length(failed_nc), max_retries,
        paste0("  ", failed_nc, collapse = "\n")
      ))
  }
  
  closeAllConnections()
  
  # ── step 5: post-download checksum verification ────────────────────────────
  cat("\n\U0001F50F Verifying downloaded files...\n")
  
  final_status <- cs_status  # start from pre-download state
  
  dl_cs <- purrr::map_chr(dl_paths, function(p) {
    if (!file.exists(p)) return("download_fail")
    result <- .verify_checksum(p)
    if (result == "mismatch") {
      warning(sprintf("\u274C Checksum mismatch after download: %s — removing.", basename(p)))
      if (file.exists(p))               file.remove(p)
      if (file.exists(paste0(p, ".md5"))) file.remove(paste0(p, ".md5"))
      return("checksum_fail")
    }
    # "ok" or "no_sidecar" — both treated as successfully downloaded
    "downloaded"
  })
  
  final_status[need_dl] <- dl_cs
  
  # ── step 6: summary ────────────────────────────────────────────────────────
  results <- tibble::tibble(
    filename   = basename(local_paths),
    local_path = local_paths,
    ftp_url    = ftp_urls,
    status     = final_status
  ) %>%
    dplyr::mutate(status = dplyr::case_when(
      status == "ok"           ~ "ok",
      status == "no_sidecar"   ~ "ok_no_sidecar",
      status == "downloaded"   ~ "downloaded",
      status == "download_fail"  ~ "download_fail",
      status == "checksum_fail"  ~ "checksum_fail",
      TRUE                     ~ status
    ))
  
  counts <- results %>% dplyr::count(status)
  cat("\n\U0001F4CA Summary:\n")
  status_icons <- c(
    ok             = "\u2705",
    ok_no_sidecar  = "\u2753",
    downloaded     = "\U0001F4E5",
    download_fail  = "\u274C",
    checksum_fail  = "\U0001F6AB"
  )
  purrr::walk2(counts$status, counts$n, function(s, n) {
    icon <- status_icons[[s]] %||% "\u2022"
    cat(sprintf("  %s  %-18s %d\n", icon, s, n))
  })
  
  # Flag any files that still need attention
  needs_retry <- results %>%
    dplyr::filter(status %in% c("download_fail", "checksum_fail"))
  if (nrow(needs_retry)) {
    cat(sprintf(
      "\n  \u21BB Re-run download_noaa_nc() to retry %d failed file(s).\n",
      nrow(needs_retry)
    ))
  }
  
  invisible(results)
}


# ===========================================================================
# USAGE EXAMPLES
# ===========================================================================

# -- SST daily for 1985 --
# results <- download_noaa_nc(
#   output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years       = 1985,
#   measure     = "sst",
#   timeframe   = "daily"
# )

# -- DHW annual maxima for 2020:2025 --
# results <- download_noaa_nc(
#   output_path  = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years        = 2020:2025,
#   measure      = "dhw",
#   timeframe    = "annual",
#   summary_type = "max"
# )

# -- Climatology only --
# results <- download_noaa_nc(
#   output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   measure     = "climatology"
# )

# -- Multiple timeframes --
# results <- download_noaa_nc(
#   output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years       = 1985,
#   measure     = "sst",
#   timeframe   = c("daily", "monthly", "annual", "climatology")
# )

# -- Inspect results --
# results %>% dplyr::filter(status %in% c("download_fail", "checksum_fail"))
# results %>% dplyr::count(status)