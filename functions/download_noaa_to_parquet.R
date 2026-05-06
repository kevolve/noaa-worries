#### Download NOAA CoralWatch 5km data and save as Parquet ####
#### (c) Kevin Novak, AIMS
#### Updated 7 May 2025

#### Install/load dependencies ####

pkgs <- c("tidyverse", "purrr", "ncdf4", "RCurl", "curl", "arrow", "fs", "terra")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p); require(p, character.only = TRUE)
  }
}))


# ===========================================================================
# HELPER UTILITIES
# ===========================================================================

#' Ensure a directory path exists, creating it if necessary.
#' Returns the path with a trailing slash.
check_path <- function(path) {
  dir <- if (stringr::str_detect(path, stringr::regex(".*\\..*"))) dirname(path) else path
  if (stringr::str_sub(dir, -1) != "/") dir <- paste0(dir, "/")
  if (!fs::dir_exists(dir)) {
    fs::dir_create(dir, recurse = TRUE)
    cat(paste0("  \U0001F4C1 Created directory: ", dir, "\n"))
  }
  dir
}

#' List files in an FTP directory.
#'
#' Returns a sorted character vector of filenames. Returns character(0) with a
#' warning (rather than crashing) when the server denies access — typically
#' because the directory path does not exist on the server.
list_ftp_files <- function(url) {
  tryCatch(
    RCurl::getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE) %>%
      stringr::str_split(pattern = "\n", simplify = TRUE) %>%
      as.character() %>%
      purrr::keep(stringr::str_detect(., "\\S")) %>%
      sort(),
    error = function(e) {
      warning(sprintf(
        "\u26A0\uFE0F  Could not list FTP directory (path may not exist):\n  %s\n  Server message: %s",
        url, conditionMessage(e)
      ))
      character(0)
    }
  )
}

#' Check available disk space (in bytes) at a given path.
#' Returns NA on failure (non-blocking).
get_free_bytes <- function(path) {
  tryCatch({
    info <- fs::fs_usage(path)
    as.numeric(info$avail)
  }, error = function(e) NA_real_)
}

#' Estimate total size (bytes) of files at a vector of FTP URLs.
#'
#' Uses curl's CURLOPT_NOBODY + CURLOPT_RANGE trick to request 0 bytes and
#' read the Content-Length from the response — reliable over FTP where plain
#' HEAD requests return no headers. Samples up to `max_check` files and
#' extrapolates to the full list.
estimate_ftp_sizes <- function(urls, max_check = 10L) {
  sample_urls <- utils::head(urls, max_check)
  sizes <- purrr::map_dbl(sample_urls, function(u) {
    tryCatch({
      h <- curl::new_handle()
      curl::handle_setopt(h, nobody = TRUE, ftp_use_epsv = FALSE)
      resp <- curl::curl_fetch_memory(u, handle = h)
      # Content-Length is in the raw headers
      raw_headers <- rawToChar(resp$headers)
      m <- regmatches(raw_headers,
                      regexpr("(?i)Content-Length:\\s*(\\d+)", raw_headers, perl = TRUE))
      if (length(m) == 0) return(NA_real_)
      as.numeric(sub("(?i)Content-Length:\\s*", "", m, perl = TRUE))
    }, error = function(e) NA_real_)
  })
  
  # Guard against all-NA (e.g. server returns no Content-Length at all)
  if (all(is.na(sizes))) {
    return(list(mean_file_bytes = NA_real_, total_est_bytes = NA_real_,
                n_sampled = length(sample_urls)))
  }
  mean_size <- mean(sizes, na.rm = TRUE)
  total_est <- mean_size * length(urls)
  list(mean_file_bytes = mean_size, total_est_bytes = total_est,
       n_sampled = length(sample_urls))
}

#' Check that sufficient disk space exists before starting a download.
#' `required_bytes` is the estimated download size; `safety_multiplier` adds
#' headroom (default 1.5× for parquet conversion overhead).
check_disk_space <- function(save_dir, required_bytes, safety_multiplier = 1.5) {
  free <- get_free_bytes(save_dir)
  if (is.na(free)) {
    warning("\u26A0\uFE0F  Could not determine free disk space at '", save_dir, "'. Proceeding anyway.")
    return(invisible(NULL))
  }
  needed <- required_bytes * safety_multiplier
  free_gb   <- round(free   / 1e9, 2)
  needed_gb <- round(needed / 1e9, 2)
  if (free < needed) {
    stop(
      sprintf(
        "\U0001F6AB Insufficient disk space!\n  Free: %.2f GB\n  Estimated needed (\u00d7%.1f safety): %.2f GB\n  Free up space or choose a different save path.",
        free_gb, safety_multiplier, needed_gb
      )
    )
  }
  cat(sprintf("  \u2705 Disk space OK: %.2f GB free, ~%.2f GB estimated needed.\n", free_gb, needed_gb))
  invisible(NULL)
}

#' Verify an .nc file against its paired .md5 sidecar.
#' Returns TRUE if valid (or no .md5 present), FALSE if mismatch.
verify_checksum <- function(nc_path) {
  md5_path <- paste0(nc_path, ".md5")
  if (!file.exists(md5_path)) return(TRUE) # nothing to check against
  
  noaa_hash <- system(paste("cat", shQuote(md5_path)), intern = TRUE) %>%
    stringr::str_extract("^\\w+")
  local_hash <- system(paste("md5 -q", shQuote(nc_path)), intern = TRUE) %>%
    stringr::str_extract("^\\w+")
  
  if (!isTRUE(all.equal(noaa_hash, local_hash))) {
    warning(sprintf(
      "\u274C Checksum MISMATCH for %s\n    NOAA:  %s\n    Local: %s\n  Removing corrupt file(s).",
      basename(nc_path), noaa_hash, local_hash
    ))
    suppressWarnings(file.remove(nc_path, md5_path))
    return(FALSE)
  }
  TRUE
}

#' Convert a single .nc file to a tidy tibble (without writing to disk).
#'
#' Uses `terra` to read the raster efficiently -- avoids the catastrophic
#' memory cost of expand.grid() across a global 5km lon x lat grid. NA cells
#' (land mask / fill values) are dropped so the output stays compact.
#'
#' A `date` column is added when `file_date` is supplied (as a Date scalar),
#' which is used when merging multiple files into one annual parquet.
#'
#' @param nc_path   Path to the .nc file.
#' @param file_date Optional Date scalar to attach as a `date` column.
#' @param drop_na   Drop rows where all value columns are NA (default TRUE).
#'
#' @return A tibble, or NULL on failure.
nc_to_tibble <- function(nc_path, file_date = NULL, drop_na = TRUE) {
  tryCatch({
    r <- terra::rast(nc_path)
    lyr_names <- names(r)
    ncells    <- terra::ncell(r)
    xy        <- terra::xyFromCell(r, seq_len(ncells))
    vals      <- terra::values(r)
    if (!is.matrix(vals)) vals <- matrix(vals, ncol = 1)
    
    tbl <- tibble::tibble(lon = xy[, 1], lat = xy[, 2])
    for (j in seq_along(lyr_names)) tbl[[lyr_names[j]]] <- vals[, j]
    
    if (drop_na && ncol(vals) > 0) {
      tbl <- tbl[rowSums(!is.na(vals)) > 0, ]
    }
    if (!is.null(file_date)) {
      tbl <- dplyr::mutate(tbl, date = as.Date(file_date), .before = 1)
    }
    tbl
  }, error = function(e) {
    warning(sprintf("\u274C nc_to_tibble failed for %s:\n  %s",
                    basename(nc_path), conditionMessage(e)))
    NULL
  })
}

#' Convert a single .nc file to a Parquet file, then remove the .nc.
#'
#' Thin wrapper around `nc_to_tibble()` for the single-file case (climatology
#' and annual summaries), where no merging across dates is needed.
#'
#' @param nc_path      Path to the downloaded .nc file.
#' @param parquet_path Destination .parquet path.
#' @param drop_na      Drop all-NA rows (default TRUE).
#'
#' @return The parquet_path on success, NA_character_ on failure.
nc_to_parquet <- function(nc_path, parquet_path, drop_na = TRUE) {
  tbl <- nc_to_tibble(nc_path, drop_na = drop_na)
  if (is.null(tbl)) return(NA_character_)
  
  check_path(dirname(parquet_path))
  arrow::write_parquet(tbl, sink = parquet_path, compression = "snappy")
  file.remove(nc_path)
  
  cat(sprintf("  \U0001F4BE Saved parquet: %s  (%s rows x %s cols)\n",
              basename(parquet_path),
              format(nrow(tbl), big.mark = ","),
              ncol(tbl)))
  parquet_path
}

#' Merge a vector of .nc files into a single annual parquet, then remove them.
#'
#' Each .nc is converted to a tibble with a `date` column parsed from its
#' filename (YYYYMMDD for daily, YYYYMM for monthly). All tibbles are stacked
#' and written as one snappy-compressed parquet file.
#'
#' @param nc_paths     Character vector of .nc paths (same year, same measure).
#' @param parquet_path Destination .parquet path.
#' @param timeframe    "daily" or "monthly" — controls the date parsing regex.
#' @param drop_na      Drop all-NA rows per slice (default TRUE).
#'
#' @return The parquet_path on success, NA_character_ on failure.
nc_merge_to_parquet <- function(nc_paths, parquet_path, timeframe, drop_na = TRUE) {
  tryCatch({
    cat(sprintf("  \U0001F4C6 Merging %d %s file(s) -> %s\n",
                length(nc_paths), timeframe, basename(parquet_path)))
    
    date_regex <- if (timeframe == "daily") "(\\d{8})(?=\\.nc$)" else "(\\d{6})(?=\\.nc$)"
    date_fmt   <- if (timeframe == "daily") "%Y%m%d"             else "%Y%m"
    
    tbls <- purrr::map(nc_paths, function(nc) {
      date_str  <- stringr::str_extract(basename(nc), date_regex)
      file_date <- as.Date(date_str, format = date_fmt)
      nc_to_tibble(nc, file_date = file_date, drop_na = drop_na)
    }) %>% purrr::compact()
    
    if (length(tbls) == 0) {
      warning(sprintf("\u274C No slices converted for %s.", basename(parquet_path)))
      return(NA_character_)
    }
    
    merged <- dplyr::bind_rows(tbls)
    
    check_path(dirname(parquet_path))
    arrow::write_parquet(merged, sink = parquet_path, compression = "snappy")
    
    # Clean up individual .nc files now the merge is written
    purrr::walk(nc_paths, function(nc) {
      if (file.exists(nc)) file.remove(nc)
    })
    
    cat(sprintf("  \U0001F4BE Saved merged parquet: %s  (%s rows x %s cols)\n",
                basename(parquet_path),
                format(nrow(merged), big.mark = ","),
                ncol(merged)))
    parquet_path
    
  }, error = function(e) {
    warning(sprintf("\u274C nc_merge_to_parquet failed for %s:\n  %s",
                    basename(parquet_path), conditionMessage(e)))
    NA_character_
  })
}


# ===========================================================================
# MAIN DOWNLOAD FUNCTION
# ===========================================================================

#' Download NOAA CRW 5km NetCDF files and save directly as Parquet.
#'
#' No intermediate .nc files are retained after successful conversion.
#' md5 checksums are downloaded alongside each .nc and verified before
#' conversion; failed checksums prompt re-download.
#'
#' @param output_path  Root folder for saved parquet files.
#' @param years        Integer vector of years (1985–present). Ignored for
#'                     climatology.
#' @param measure      One of: "climatology", "sst", "dhw", "ssta", "baa",
#'                     "baa5", "hs", "baa-max-7d", "sst-trend-7d".
#' @param summary_type NULL/"all" or one/more of "mean", "min", "max".
#' @param timeframe    One or more of "daily", "monthly", "annual",
#'                     "climatology".
#' @param max_retries  Max retry loops before giving up on stubborn files.
#' @param safety_multiplier  Multiplier for disk-space headroom check (default 1.5).
#'
#' @return Invisibly returns a tibble summarising each file's outcome
#'         (url, parquet_path, checksum_ok, converted).
download_noaa_parquet <- function(
    output_path  = "/Users/uqkbairo/MODRRAP/noaa-worries",
    years        = NULL,
    measure      = c("sst", "dhw", "climatology"),
    summary_type = NULL,
    timeframe    = c("daily", "monthly", "annual", "climatology"),
    max_retries  = 5L,
    safety_multiplier = 1.5
) {
  
  # ---- argument tidying ----
  measure   <- match.arg(measure[1],
                         c("climatology", "baa", "baa-max-7d", "baa5", "baa5-max-7d",
                           "dhw", "hs", "sst", "sst-trend-7d", "ssta", "year-to-date"))
  timeframe <- match.arg(timeframe,
                         c("annual", "monthly", "daily", "climatology"),
                         several.ok = TRUE)
  
  if (is.null(summary_type) || identical(summary_type, "all")) {
    summary_type <- "all"
  } else {
    bad_st <- setdiff(summary_type, c("mean", "min", "max"))
    if (length(bad_st) > 0)
      stop("Invalid summary_type(s): ", paste(bad_st, collapse = ", "),
           "\nUse: 'all', 'mean', 'min', or 'max'.")
  }
  
  # ---- valid measure × timeframe combos ----
  daily_ok   <- c("baa","baa-max-7d","baa5","baa5-max-7d","dhw","hs","sst","sst-trend-7d","ssta","year-to-date")
  monthly_ok <- c("baa","baa5","dhw","hs","sst","ssta")
  annual_ok  <- c("baa","baa5","dhw","hs","sst","ssta")
  
  if ("daily"   %in% timeframe && !measure %in% c(daily_ok,  "climatology"))
    stop("measure '", measure, "' is not available at daily resolution.")
  if ("monthly" %in% timeframe && !measure %in% c(monthly_ok, "climatology"))
    stop("measure '", measure, "' is not available at monthly resolution.")
  if ("annual"  %in% timeframe && !measure %in% c(annual_ok,  "climatology"))
    stop("measure '", measure, "' is not available at annual resolution.")
  
  if (measure == "climatology") {
    timeframe <- "climatology"
    message("\u2139\uFE0F  measure = 'climatology' \u2192 timeframe forced to 'climatology'.")
  }
  
  if (any(timeframe != "climatology")) {
    if (is.null(years))
      stop("years must be specified for timeframe(s): ",
           paste(setdiff(timeframe, "climatology"), collapse = ", "))
    invalid_yrs <- years[!years %in% 1985:2025]
    if (length(invalid_yrs) > 0)
      stop("Year(s) out of range [1985–2025]: ", paste(invalid_yrs, collapse = ", "))
  }
  
  # ---- paths ----
  base_url    <- "ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/"
  output_path <- check_path(output_path)
  
  # ---- collect FTP URLs + local save paths ----
  cat("\U0001F50D Querying NOAA FTP server for file listings...\n")
  
  url_file_paths  <- character(0)
  save_file_paths <- character(0)   # .nc paths (temporary during download)
  parquet_paths   <- character(0)   # final .parquet destinations
  
  # Per-file metadata tracked alongside the file path vectors
  file_years      <- integer(0)
  file_timeframes <- character(0)
  
  # Helper: register a batch of filenames with their year + timeframe.
  #
  # Parquet grouping strategy:
  #   daily   — one parquet per month (YYYY_MM), so 12 merged files per year.
  #             Daily filenames always end in YYYYMMDD.nc; the YYYYMM is
  #             extracted to form the group key.
  #   monthly — one parquet per year, merging all 12 monthly slices.
  #   annual / climatology — one parquet per source .nc (no merging needed).
  register_files <- function(folder_url, filenames, local_dir, yr, tf) {
    url_file_paths  <<- c(url_file_paths,  paste0(folder_url, filenames))
    nc_paths         <- paste0(local_dir,   filenames)
    save_file_paths <<- c(save_file_paths, nc_paths)
    
    pq_paths <- if (tf == "daily") {
      # Extract YYYYMM from the trailing 8-digit date in each filename.
      # Works for both coraltemp_v3.1_YYYYMMDD.nc and ct5km_*_YYYYMMDD.nc.
      yyyymm <- stringr::str_extract(filenames, "\\d{8}(?=\\.nc$)") %>%
        stringr::str_sub(1, 6)
      paste0(local_dir, measure, "_daily_", yyyymm, ".parquet")
      
    } else if (tf == "monthly") {
      # One parquet per year merging all 12 monthly slices.
      rep(paste0(local_dir, measure, "_monthly_", yr, ".parquet"), length(filenames))
      
    } else if (tf == "annual") {
      # Extract the 4-digit year and optional summary type from each filename.
      # e.g. ct5km_sst-mean_v3.1_1985.nc  ->  sst_annual_mean_1985.parquet
      #      ct5km_dhw-max_v3.1_1985.nc   ->  dhw_annual_max_1985.parquet
      yr_str  <- stringr::str_extract(filenames, "\\d{4}(?=\\.nc$)")
      st_str  <- stringr::str_extract(filenames, "(?<=-)(?:mean|min|max)(?=_)")
      st_part <- dplyr::if_else(is.na(st_str), "", paste0("_", st_str))
      paste0(local_dir, measure, "_annual", st_part, "_", yr_str, ".parquet")
      
    } else {
      # climatology: single file, no year component.
      # e.g. ct5km_climatology_v3.1_1985-2012_dhw_*.nc  ->  sst_climatology.parquet
      rep(paste0(local_dir, measure, "_climatology.parquet"), length(filenames))
    }
    
    parquet_paths   <<- c(parquet_paths,   pq_paths)
    file_years      <<- c(file_years,      rep(yr, length(filenames)))
    file_timeframes <<- c(file_timeframes, rep(tf, length(filenames)))
  }
  
  # -- climatology --
  if ("climatology" %in% timeframe) {
    cat("  Fetching climatology listing...\n")
    folder_url <- paste0(base_url, "climatology/")
    local_dir  <- check_path(paste0(output_path, "climatology/"))
    filenames  <- list_ftp_files(folder_url) %>% stringr::str_subset("\\.nc$")
    if (length(filenames) > 1)
      stop("Unexpected: multiple .nc files found in /climatology/ on FTP server.")
    register_files(folder_url, filenames, local_dir, yr = NA_integer_, tf = "climatology")
  }
  
  # Helper: build exact-match filename pattern for a given measure.
  #
  # Daily SST is a special case: NOAA distributes it as CoralTemp with the
  # prefix "coraltemp_" rather than "ct5km_sst_", e.g.:
  #   coraltemp_v3.1_19850101.nc
  # All other measures (baa, dhw, hs, ssta, etc.) and all monthly/annual SST
  # files use the standard ct5km_ prefix, e.g.:
  #   ct5km_dhw_v3.1_19850101.nc
  #   ct5km_sst-mean_v3.1_1985.nc
  #
  # The pattern anchors on the character immediately after the measure token
  # (always _ or -) so "sst" never accidentally matches "ssta" files.
  make_measure_pattern <- function(m, is_daily = FALSE) {
    if (m == "sst" && is_daily) {
      "^coraltemp_"
    } else {
      paste0("^ct5km_", stringr::str_replace_all(m, "-", "[-_]"), "[_-]")
    }
  }
  
  # -- annual --
  if ("annual" %in% timeframe) {
    cat("  Fetching annual listing...\n")
    folder_url <- paste0(base_url, "nc/v1.0/annual/")
    local_dir  <- check_path(paste0(output_path, measure, "/annual/"))
    filenames  <- list_ftp_files(folder_url) %>% stringr::str_subset("\\.nc$")
    
    # Filter to exact measure (anchored to avoid e.g. sst matching ssta)
    filenames <- filenames %>% stringr::str_subset(make_measure_pattern(measure))
    
    # Filter to requested summary_type
    if (summary_type != "all") {
      st_pattern <- paste0(paste0("-", summary_type, "[_.]"), collapse = "|")
      filenames  <- filenames %>% stringr::str_subset(st_pattern)
    }
    
    # Filter to requested years — annual filenames contain the year as a
    # 4-digit token, e.g. ct5km_sst-mean_v3.1_1985.nc
    yr_pattern <- paste(years, collapse = "|")
    filenames  <- filenames %>%
      stringr::str_subset(paste0("(?:^|[_.])(", yr_pattern, ")(?:[_.]|$)"))
    
    if (length(filenames) == 0)
      warning("\u26A0\uFE0F  No annual files matched measure='", measure,
              "', summary_type='", summary_type,
              "', years=", paste(range(years), collapse = "\u2013"), ".")
    else
      register_files(folder_url, filenames, local_dir, yr = NA_integer_, tf = "annual")
  }
  
  # -- monthly --
  # FTP path pattern: nc/v1.0/monthly/<year>/   (no measure subfolder)
  # Files are filtered by measure pattern in the filename after listing.
  if ("monthly" %in% timeframe) {
    local_dir <- check_path(paste0(output_path, measure, "/monthly/"))
    for (yr in years) {
      cat(sprintf("  Fetching monthly listing for %d...\n", yr))
      folder_url <- paste0(base_url, "nc/v1.0/monthly/", yr, "/")
      filenames  <- list_ftp_files(folder_url) %>%
        stringr::str_subset("\\.nc$") %>%
        stringr::str_subset(make_measure_pattern(measure))
      if (summary_type != "all") {
        st_pattern <- paste0(paste0("^ct5km_", measure, "-", summary_type, "_"), collapse = "|")
        filenames  <- filenames %>% stringr::str_subset(st_pattern)
      }
      if (length(filenames) > 0)
        register_files(folder_url, filenames, local_dir, yr = yr, tf = "monthly")
      else
        warning(sprintf("\u26A0\uFE0F  No monthly files found for measure='%s', year=%d.", measure, yr))
    }
  }
  
  # -- daily --
  # FTP path pattern: nc/v1.0/daily/<measure>/<year>/
  if ("daily" %in% timeframe) {
    local_dir <- check_path(paste0(output_path, measure, "/daily/"))
    for (yr in years) {
      cat(sprintf("  Fetching daily listing for %d...\n", yr))
      folder_url <- paste0(base_url, "nc/v1.0/daily/", measure, "/", yr, "/")
      filenames  <- list_ftp_files(folder_url) %>%
        stringr::str_subset("\\.nc$") %>%
        stringr::str_subset(make_measure_pattern(measure, is_daily = TRUE))
      if (length(filenames) > 0)
        register_files(folder_url, filenames, local_dir, yr = yr, tf = "daily")
      else
        warning(sprintf("\u26A0\uFE0F  No daily files found for measure='%s', year=%d.", measure, yr))
    }
  }
  
  # ---- skip files that are already resolved ----
  # A file is skipped if EITHER:
  #   (a) its merged/final parquet already exists on disk -- fully done, OR
  #   (b) its .nc already exists on disk -- partially downloaded in a previous
  #       run (incomplete daily group); curl resume will complete it.
  # In case (b) the .nc is still in the download list so curl can resume it,
  # but it will not be re-downloaded from scratch.
  parquet_exists <- file.exists(parquet_paths)
  nc_exists      <- file.exists(save_file_paths)
  
  already_done <- parquet_exists   # fully converted groups -> skip entirely
  
  if (any(already_done)) {
    n_pq_done <- length(unique(parquet_paths[already_done]))
    cat(sprintf(
      "⏭️  Skipping %d source file(s) (%d parquet(s) already exist).
",
      sum(already_done), n_pq_done
    ))
  }
  if (any(nc_exists & !already_done)) {
    cat(sprintf(
      "⏳ %d .nc file(s) already on disk from a previous partial run -- will resume/verify.
",
      sum(nc_exists & !already_done)
    ))
  }
  
  url_todo        <- url_file_paths[!already_done]
  nc_todo         <- save_file_paths[!already_done]
  parquet_todo    <- parquet_paths[!already_done]
  
  # Also include the .md5 sidecar URLs
  md5_urls  <- paste0(url_todo, ".md5")
  md5_paths <- paste0(nc_todo,  ".md5")
  
  n_total <- length(url_todo)
  if (n_total == 0) {
    cat("\u2705 All requested files are already present as parquet. Nothing to do!\n")
    return(invisible(tibble::tibble(
      url          = url_file_paths,
      parquet_path = parquet_paths,
      skipped      = already_done,
      checksum_ok  = NA,
      converted    = already_done,
      incomplete   = FALSE
    )))
  }
  
  cat(sprintf("\U0001F4CB %d file(s) to download (+ .md5 sidecars).\n", n_total))
  
  # ---- disk space pre-flight ----
  cat("\U0001F4BE Estimating download size and checking disk space...\n")
  size_est <- estimate_ftp_sizes(url_todo, max_check = min(10L, n_total))
  if (!is.na(size_est$total_est_bytes)) {
    cat(sprintf("  \u2248 Mean file size: %.1f MB | Total estimate: %.1f GB\n",
                size_est$mean_file_bytes / 1e6,
                size_est$total_est_bytes / 1e9))
    check_disk_space(output_path, size_est$total_est_bytes, safety_multiplier)
  } else {
    warning("\u26A0\uFE0F  Could not estimate file sizes; skipping disk space check.")
  }
  
  # ---- download loop (nc + md5 together, with retries) ----
  cat(sprintf("\U0001F4E5 Starting download of %d file(s)...\n\n", n_total))
  closeAllConnections()
  start_tm <- Sys.time()
  
  # Interleave .nc and .md5 downloads
  all_dl_urls  <- c(url_todo, md5_urls)
  all_dl_paths <- c(nc_todo,  md5_paths)
  
  remaining_url  <- all_dl_urls
  remaining_path <- all_dl_paths
  n_fails        <- 0L
  
  while (length(remaining_url) > 0 && n_fails <= max_retries) {
    dl <- curl::multi_download(
      urls      = remaining_url,
      destfiles = remaining_path,
      resume    = TRUE
    )
    done_i         <- which(file.exists(remaining_path))
    remaining_url  <- remaining_url[ -done_i]
    remaining_path <- remaining_path[-done_i]
    
    if (length(remaining_url) == 0) break
    
    n_fails <- n_fails + 1L
    cat(sprintf("  \u21BB %d file(s) not yet downloaded; retry %d/%d...\n",
                length(remaining_url), n_fails, max_retries))
    Sys.sleep(10)
  }
  
  if (length(remaining_url) > 0) {
    warning(sprintf("\u274C %d file(s) could not be downloaded after %d retries:\n%s",
                    length(remaining_url), max_retries,
                    paste0("  ", remaining_url, collapse = "\n")))
  }
  
  # ---- checksum + conversion ----
  cat("\n\U0001F50F Verifying checksums and converting to parquet...\n")
  
  # Subset metadata vectors to the todo set
  todo_years      <- file_years[!already_done]
  todo_timeframes <- file_timeframes[!already_done]
  
  # Per-file checksum verification first
  checksum_ok_vec <- purrr::map_lgl(nc_todo, function(nc) {
    if (!file.exists(nc)) return(NA)
    verify_checksum(nc)
  })
  
  # Build a data frame of all todo files with their group key
  todo_df <- tibble::tibble(
    nc            = nc_todo,
    pq            = parquet_todo,
    url           = url_todo,
    year          = todo_years,
    tf            = todo_timeframes,
    checksum_ok   = checksum_ok_vec,
    downloaded    = file.exists(nc_todo)
  )
  
  # ---- Single-file conversion (climatology + annual) ----
  single_df <- todo_df %>% dplyr::filter(tf %in% c("climatology", "annual"))
  
  single_results <- purrr::pmap_dfr(single_df, function(nc, pq, url, year, tf,
                                                        checksum_ok, downloaded, ...) {
    if (!downloaded)
      return(tibble::tibble(url = url, parquet_path = pq,
                            skipped = FALSE, checksum_ok = NA,
                            converted = FALSE, incomplete = FALSE))
    converted <- FALSE
    if (isTRUE(checksum_ok)) {
      result    <- nc_to_parquet(nc, pq)
      converted <- !is.na(result)
    } else {
      cat(sprintf("  \u26A0\uFE0F Skipping %s (checksum failed).\n", basename(nc)))
    }
    tibble::tibble(url = url, parquet_path = pq,
                   skipped = FALSE, checksum_ok = checksum_ok,
                   converted = converted, incomplete = FALSE)
  })
  
  # ---- Merged conversion (daily + monthly: group by year x parquet path) ----
  merge_df <- todo_df %>% dplyr::filter(tf %in% c("daily", "monthly"))
  
  merge_results <- if (nrow(merge_df) == 0) {
    tibble::tibble(url = character(), parquet_path = character(),
                   skipped = logical(), checksum_ok = logical(), converted = logical(),
                   incomplete = logical())
  } else {
    merge_df %>%
      dplyr::group_by(pq, year, tf) %>%
      dplyr::summarise(
        url         = dplyr::first(url),
        n_expected  = dplyr::n(),
        nc_valid    = list(nc[downloaded & !isFALSE(checksum_ok)]),
        n_valid     = sum(downloaded & !isFALSE(checksum_ok)),
        any_bad_cs  = any(isFALSE(checksum_ok)),
        any_missing = any(!downloaded),
        .groups     = "drop"
      ) %>%
      { purrr::map_dfr(seq_len(nrow(.)), function(i) {
        row         <- .[i, ]
        pq          <- row$pq
        tf          <- row$tf
        url         <- row$url
        any_bad_cs  <- row$any_bad_cs
        any_missing <- row$any_missing
        n_expected  <- row$n_expected
        n_valid     <- row$n_valid
        ncs         <- unlist(row$nc_valid)
        
        # ── Case 1: nothing downloaded at all — record failure, nothing to keep ──
        if (length(ncs) == 0) {
          cat(sprintf(
            "  \u274C No files downloaded for %s \u2014 will retry on next run.\n",
            basename(pq)
          ))
          return(tibble::tibble(url = url, parquet_path = pq,
                                skipped = FALSE, checksum_ok = !any_bad_cs,
                                converted = FALSE, incomplete = TRUE))
        }
        
        # ── Case 2: some files missing — defer merge, keep .nc files for retry ──
        # The already-downloaded .nc files stay on disk. On re-run, the missing
        # files will be downloaded and file.exists() will skip the existing ones,
        # after which the complete group will be merged.
        if (any_missing) {
          cat(sprintf(
            "  \u23F3 Incomplete: %s has %d/%d files \u2014 keeping .nc files, retry on next run.\n",
            basename(pq), n_valid, n_expected
          ))
          return(tibble::tibble(url = url, parquet_path = pq,
                                skipped = FALSE, checksum_ok = !any_bad_cs,
                                converted = FALSE, incomplete = TRUE))
        }
        
        # ── Case 3: all expected files present — merge now ──
        if (any_bad_cs)
          cat(sprintf(
            "  \u26A0\uFE0F %s: checksum failure(s) present \u2014 merging %d valid file(s).\n",
            basename(pq), n_valid
          ))
        result    <- nc_merge_to_parquet(ncs, pq, timeframe = tf)
        converted <- !is.na(result)
        
        # If the merge itself failed, preserve .nc files for retry
        if (!converted)
          cat(sprintf(
            "  \u26A0\uFE0F Merge failed for %s \u2014 keeping .nc files for retry.\n",
            basename(pq)
          ))
        
        tibble::tibble(url = url, parquet_path = pq,
                       skipped = FALSE, checksum_ok = !any_bad_cs,
                       converted = converted, incomplete = !converted)
      })
      }
  }
  
  results <- dplyr::bind_rows(single_results, merge_results)
  
  # Append already-done rows
  results <- dplyr::bind_rows(
    results,
    tibble::tibble(
      url          = url_file_paths[already_done],
      parquet_path = parquet_paths[already_done],
      skipped      = TRUE,
      checksum_ok  = TRUE,
      converted    = TRUE,
      incomplete   = FALSE
    )
  )
  
  # ---- summary ----
  runtime      <- round(as.numeric(difftime(Sys.time(), start_tm, units = "mins")), 2)
  n_ok         <- sum(results$converted,   na.rm = TRUE)
  n_bad        <- sum(!results$checksum_ok, na.rm = TRUE)
  n_skip       <- sum(results$skipped,     na.rm = TRUE)
  n_incomplete <- sum(results$incomplete,  na.rm = TRUE)
  
  cat(sprintf(
    "\n\U0001F389 Done in %.1f min!\n  \u2705 Converted:         %d\n  \u23ED\uFE0F Already done:     %d\n  \u23F3 Incomplete groups: %d (re-run to retry)\n  \u274C Failed/corrupt:    %d\n",
    runtime, n_ok, n_skip, n_incomplete, n_bad
  ))
  
  if (n_bad > 0) {
    cat("  Failed files (re-run to retry):\n")
    results %>%
      dplyr::filter(!checksum_ok | !converted) %>%
      dplyr::pull(url) %>%
      purrr::walk(~ cat("    ", .x, "\n"))
  }
  
  closeAllConnections()
  invisible(results)
}


# ===========================================================================
# USAGE EXAMPLES
# ===========================================================================

# -- SST: daily + monthly + annual + climatology for 1985 --
# results <- download_noaa_parquet(
#   output_path  = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years        = 1985,
#   measure      = "sst",
#   timeframe    = c("daily", "monthly", "annual", "climatology"),
#   summary_type = "mean"
# )

# -- DHW: annual maxima for 2020–2025 --
# results <- download_noaa_parquet(
#   output_path  = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years        = 2020:2025,
#   measure      = "dhw",
#   timeframe    = "annual",
#   summary_type = "max"
# )

# -- Climatology only --
# results <- download_noaa_parquet(
#   output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   measure     = "climatology"
# )

# -- Inspect results tibble --
# results %>% dplyr::filter(!converted)   # any failures?
# results %>% dplyr::count(checksum_ok)   # checksum summary