#### Audit local NOAA CoralWatch 5km data against the FTP server ####
#### (c) Kevin Novak, AIMS
#### Updated 7 May 2025
####
#### Workflow:
####   1. audit_noaa_data()  — scans local folder vs FTP, returns an audit tibble
####   2. review_audit()     — prints a human-readable summary and prompts the user
####                           to confirm which flagged files to queue for download
####   3. Pass the returned queue into download_noaa_parquet() to fill the gaps

#### Dependencies ####

pkgs <- c("tidyverse", "purrr", "RCurl", "curl", "arrow", "fs", "cli")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p); require(p, character.only = TRUE)
  }
}))

# Source the download script so its helpers are available
# (adjust path if scripts live in a different location)
# source("download_noaa_to_parquet.R")


# ===========================================================================
# INTERNAL HELPERS  (duplicated here so the audit script is self-contained)
# ===========================================================================

.check_path <- function(path) {
  dir <- if (stringr::str_detect(path, stringr::regex(".*\\..*"))) dirname(path) else path
  if (stringr::str_sub(dir, -1) != "/") dir <- paste0(dir, "/")
  if (!fs::dir_exists(dir)) fs::dir_create(dir, recurse = TRUE)
  dir
}

.list_ftp_files <- function(url) {
  tryCatch(
    RCurl::getURL(url, ftp.use.epsv = FALSE, crlf = TRUE, dirlistonly = TRUE) %>%
      stringr::str_split(pattern = "\n", simplify = TRUE) %>%
      as.character() %>%
      purrr::keep(stringr::str_detect(., "\\S")) %>%
      sort(),
    error = function(e) {
      warning("Could not list FTP directory: ", url, "\n  ", conditionMessage(e))
      character(0)
    }
  )
}

# Read the NOAA-supplied hash from a local .md5 sidecar file
.read_noaa_hash <- function(md5_path) {
  if (!file.exists(md5_path)) return(NA_character_)
  system(paste("cat", shQuote(md5_path)), intern = TRUE) %>%
    stringr::str_extract("^\\w+")
}

# Compute the md5 hash of a local file (macOS md5 -q)
.local_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  system(paste("md5 -q", shQuote(path)), intern = TRUE) %>%
    stringr::str_extract("^\\w+")
}

# Fetch the NOAA hash directly from the FTP .md5 file (no local copy needed)
.fetch_ftp_hash <- function(md5_url) {
  tryCatch(
    RCurl::getURL(md5_url, ftp.use.epsv = FALSE) %>%
      stringr::str_extract("^\\w+"),
    error = function(e) NA_character_
  )
}

# Validate a parquet file: can arrow open it and does it have >0 rows?
.validate_parquet <- function(pq_path) {
  if (!file.exists(pq_path)) return(FALSE)
  tryCatch({
    info <- arrow::open_dataset(pq_path, format = "parquet")
    nrow(dplyr::collect(info)) > 0
  }, error = function(e) FALSE)
}


# ===========================================================================
# BUILD THE EXPECTED FILE MANIFEST FROM THE FTP SERVER
# ===========================================================================

#' Build a tibble of every expected file on the NOAA FTP server for a given
#' combination of measure × timeframe × years, alongside the local parquet path.
#'
#' @inheritParams download_noaa_parquet
#' @return A tibble with columns:
#'   measure, timeframe, year, filename, ftp_url, md5_url,
#'   local_parquet, local_md5_sidecar
build_manifest <- function(
    output_path  = "/Users/uqkbairo/MODRRAP/noaa-worries",
    years        = 1985:2025,
    measure      = c("sst", "dhw", "climatology"),
    summary_type = NULL,
    timeframe    = c("daily", "monthly", "annual", "climatology")
) {
  measure   <- match.arg(measure[1],
                         c("climatology","baa","baa-max-7d","baa5","baa5-max-7d",
                           "dhw","hs","sst","sst-trend-7d","ssta","year-to-date"))
  timeframe <- match.arg(timeframe,
                         c("annual","monthly","daily","climatology"),
                         several.ok = TRUE)
  if (is.null(summary_type) || identical(summary_type, "all")) summary_type <- "all"
  if (measure == "climatology") timeframe <- "climatology"

  base_url    <- "ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/"
  output_path <- .check_path(output_path)

  rows <- list()

  # ---- climatology ----
  if ("climatology" %in% timeframe) {
    cat("\U0001F50D Fetching climatology listing from FTP...\n")
    folder_url <- paste0(base_url, "climatology/")
    local_dir  <- .check_path(paste0(output_path, "climatology/"))
    fnames <- .list_ftp_files(folder_url) %>% stringr::str_subset("\\.nc$")
    if (length(fnames) > 0) {
      rows[["climatology"]] <- tibble::tibble(
        measure    = "sst",
        timeframe  = "climatology",
        year       = NA_integer_,
        filename   = fnames,
        ftp_url    = paste0(folder_url, fnames),
        md5_url    = paste0(folder_url, fnames, ".md5"),
        local_parquet      = paste0(local_dir, sub("\\.nc$", ".parquet", fnames)),
        local_md5_sidecar  = paste0(local_dir, fnames, ".md5")
      )
    }
  }

  # ---- annual ----
  if ("annual" %in% timeframe) {
    cat("\U0001F50D Fetching annual listing from FTP...\n")
    folder_url <- paste0(base_url, "nc/v1.0/annual/")
    local_dir  <- .check_path(paste0(output_path, measure, "/annual/"))
    fnames <- .list_ftp_files(folder_url) %>% stringr::str_subset("\\.nc$") %>%
      stringr::str_subset(paste0("^ct5km_", stringr::str_replace_all(measure, "-", "[-_]")))
    if (summary_type != "all")
      fnames <- fnames %>%
        stringr::str_subset(paste0(paste0("-", summary_type, "[_.]"), collapse = "|"))
    if (length(fnames) > 0) {
      rows[["annual"]] <- tibble::tibble(
        measure    = measure,
        timeframe  = "annual",
        year       = NA_integer_,
        filename   = fnames,
        ftp_url    = paste0(folder_url, fnames),
        md5_url    = paste0(folder_url, fnames, ".md5"),
        local_parquet      = paste0(local_dir, sub("\\.nc$", ".parquet", fnames)),
        local_md5_sidecar  = paste0(local_dir, fnames, ".md5")
      )
    }
  }

  # ---- monthly ----
  if ("monthly" %in% timeframe) {
    local_dir <- .check_path(paste0(output_path, measure, "/monthly/"))
    monthly_rows <- purrr::map_dfr(years, function(yr) {
      cat(sprintf("  \U0001F50D Fetching monthly listing for %d...\n", yr))
      folder_url <- paste0(base_url, "nc/v1.0/monthly/", yr, "/")
      fnames <- .list_ftp_files(folder_url) %>% stringr::str_subset("\\.nc$") %>%
        stringr::str_subset(paste0("ct5km_", measure))
      if (summary_type != "all")
        fnames <- fnames %>%
          stringr::str_subset(paste0(paste0("-", summary_type, "[_.]"), collapse = "|"))
      if (length(fnames) == 0) return(tibble::tibble())
      tibble::tibble(
        measure    = measure,
        timeframe  = "monthly",
        year       = yr,
        filename   = fnames,
        ftp_url    = paste0(folder_url, fnames),
        md5_url    = paste0(folder_url, fnames, ".md5"),
        local_parquet      = paste0(local_dir, sub("\\.nc$", ".parquet", fnames)),
        local_md5_sidecar  = paste0(local_dir, fnames, ".md5")
      )
    })
    if (nrow(monthly_rows) > 0) rows[["monthly"]] <- monthly_rows
  }

  # ---- daily ----
  if ("daily" %in% timeframe) {
    local_dir <- .check_path(paste0(output_path, measure, "/daily/"))
    daily_rows <- purrr::map_dfr(years, function(yr) {
      cat(sprintf("  \U0001F50D Fetching daily listing for %d...\n", yr))
      folder_url <- paste0(base_url, "nc/v1.0/daily/", measure, "/", yr, "/")
      fnames <- .list_ftp_files(folder_url) %>% stringr::str_subset("\\.nc$")
      if (length(fnames) == 0) return(tibble::tibble())
      tibble::tibble(
        measure    = measure,
        timeframe  = "daily",
        year       = yr,
        filename   = fnames,
        ftp_url    = paste0(folder_url, fnames),
        md5_url    = paste0(folder_url, fnames, ".md5"),
        local_parquet      = paste0(local_dir, sub("\\.nc$", ".parquet", fnames)),
        local_md5_sidecar  = paste0(local_dir, fnames, ".md5")
      )
    })
    if (nrow(daily_rows) > 0) rows[["daily"]] <- daily_rows
  }

  dplyr::bind_rows(rows)
}


# ===========================================================================
# AUDIT FUNCTION
# ===========================================================================

#' Audit the local data folder against the NOAA FTP manifest.
#'
#' For each expected file, records:
#'   - whether the local parquet exists and is readable
#'   - whether a local .md5 sidecar exists
#'   - whether the local parquet hash matches the NOAA-supplied hash
#'     (fetched live from FTP when the sidecar is absent)
#'
#' @param manifest  A tibble produced by `build_manifest()`, or NULL to call
#'                  `build_manifest()` automatically using the other arguments.
#' @param verify_hashes  If TRUE (default), check md5 hashes for every parquet
#'                  that exists locally. Set FALSE for a fast existence-only scan.
#' @param ftp_hash_fallback  If TRUE, fetch the .md5 from FTP when no local
#'                  sidecar is present (slower, but more thorough). Default TRUE.
#' @inheritParams build_manifest
#'
#' @return A tibble (the "audit") with one row per expected file and columns:
#'   measure, timeframe, year, filename, ftp_url,
#'   local_parquet, parquet_exists, parquet_readable,
#'   md5_sidecar_exists, noaa_hash, local_hash, hash_match,
#'   status   ["ok" | "missing" | "unreadable" | "hash_mismatch" | "no_checksum"]
audit_noaa_data <- function(
    output_path       = "/Users/uqkbairo/MODRRAP/noaa-worries",
    years             = 1985:2025,
    measure           = c("sst", "dhw", "climatology"),
    summary_type      = NULL,
    timeframe         = c("daily", "monthly", "annual", "climatology"),
    manifest          = NULL,
    verify_hashes     = TRUE,
    ftp_hash_fallback = TRUE
) {
  if (is.null(manifest)) {
    manifest <- build_manifest(
      output_path  = output_path,
      years        = years,
      measure      = measure,
      summary_type = summary_type,
      timeframe    = timeframe
    )
  }

  n <- nrow(manifest)
  cat(sprintf("\n\U0001F4CB Auditing %d expected file(s)...\n", n))

  audit <- manifest %>%
    dplyr::mutate(
      # --- existence ---
      parquet_exists   = file.exists(local_parquet),
      parquet_readable = purrr::map_lgl(local_parquet, .validate_parquet),
      md5_sidecar_exists = file.exists(local_md5_sidecar),

      # --- hashes ---
      noaa_hash = dplyr::case_when(
        md5_sidecar_exists               ~ purrr::map_chr(local_md5_sidecar, .read_noaa_hash),
        ftp_hash_fallback & parquet_exists ~ purrr::map_chr(md5_url, .fetch_ftp_hash),
        TRUE                             ~ NA_character_
      ),
      local_hash = dplyr::if_else(
        verify_hashes & parquet_exists,
        purrr::map_chr(local_parquet, .local_md5),
        NA_character_
      ),
      hash_match = dplyr::case_when(
        is.na(noaa_hash) | is.na(local_hash) ~ NA,
        TRUE                                 ~ noaa_hash == local_hash
      ),

      # --- status label ---
      status = dplyr::case_when(
        !parquet_exists                      ~ "missing",
        !parquet_readable                    ~ "unreadable",
        isFALSE(hash_match)                  ~ "hash_mismatch",
        is.na(hash_match)                    ~ "no_checksum",
        TRUE                                 ~ "ok"
      )
    ) %>%
    dplyr::select(
      measure, timeframe, year, filename,
      ftp_url, md5_url, local_parquet,
      parquet_exists, parquet_readable,
      md5_sidecar_exists, noaa_hash, local_hash, hash_match, status
    )

  # Print headline counts
  counts <- audit %>% dplyr::count(status)
  cat("\n\U0001F4CA Audit summary:\n")
  purrr::walk2(counts$status, counts$n, function(s, n) {
    icon <- switch(s,
      ok            = "\u2705",
      missing       = "\u274C",
      unreadable    = "\U0001F6AB",
      hash_mismatch = "\u26A0\uFE0F",
      no_checksum   = "\u2753",
      "\u2022"
    )
    cat(sprintf("  %s  %-16s %d\n", icon, s, n))
  })
  cat("\n")

  invisible(audit)
}


# ===========================================================================
# INTERACTIVE REVIEW + QUEUE BUILDER
# ===========================================================================

#' Print a detailed audit report, ask the user which issue categories to
#' re-download, then return a filtered manifest ready to pass to
#' `download_noaa_parquet()`.
#'
#' Statuses offered for user selection:
#'   [M] missing       — file never downloaded
#'   [U] unreadable    — parquet exists but arrow cannot open it
#'   [H] hash_mismatch — local hash differs from NOAA reference
#'   [N] no_checksum   — parquet present but no hash available to verify
#'
#' @param audit   Tibble returned by `audit_noaa_data()`.
#' @param auto    Character vector of status codes to queue automatically
#'                without prompting (e.g. c("missing","hash_mismatch")).
#'                NULL (default) = always prompt interactively.
#' @param delete_corrupt  If TRUE, delete local parquet files flagged as
#'                "unreadable" or "hash_mismatch" before returning the queue,
#'                so `download_noaa_parquet()` treats them as fresh downloads.
#'
#' @return A tibble with the same columns as the manifest (ftp_url, local
#'         paths, etc.) for every file the user elected to re-download.
#'         Pass this as the `manifest` argument of `download_noaa_parquet()`,
#'         or extract `ftp_url` and `local_parquet` vectors manually.
review_audit <- function(
    audit,
    auto           = NULL,
    delete_corrupt = TRUE
) {
  problem_statuses <- c("missing", "unreadable", "hash_mismatch", "no_checksum")
  problems <- audit %>% dplyr::filter(status %in% problem_statuses)

  if (nrow(problems) == 0) {
    cat("\U0001F389 All files are present and verified — nothing to re-download!\n")
    return(invisible(audit[0, ]))
  }

  # ---- detailed table per status ----
  cli::cli_h1("NOAA Data Audit Review")

  status_meta <- tibble::tribble(
    ~status,         ~code, ~label,                                          ~icon,
    "missing",       "M",   "Missing (never downloaded)",                    "\u274C",
    "unreadable",    "U",   "Unreadable (corrupt parquet)",                  "\U0001F6AB",
    "hash_mismatch", "H",   "Hash mismatch (differs from NOAA reference)",   "\u26A0\uFE0F",
    "no_checksum",   "N",   "No checksum (cannot verify integrity)",          "\u2753"
  )

  present_statuses <- dplyr::intersect(problem_statuses, unique(problems$status))
  status_meta      <- status_meta %>% dplyr::filter(status %in% present_statuses)

  for (i in seq_len(nrow(status_meta))) {
    s    <- status_meta$status[i]
    icon <- status_meta$icon[i]
    lbl  <- status_meta$label[i]
    grp  <- problems %>% dplyr::filter(status == s)

    cli::cli_h2(sprintf("%s  [%s] %s  (%d file(s))",
                        icon, status_meta$code[i], lbl, nrow(grp)))

    grp %>%
      dplyr::arrange(timeframe, year, filename) %>%
      dplyr::select(timeframe, year, filename) %>%
      print(n = Inf)
    cat("\n")
  }

  # ---- prompt ----
  if (!is.null(auto)) {
    # Non-interactive mode: queue the supplied statuses automatically
    chosen_statuses <- intersect(auto, present_statuses)
    cat(sprintf(
      "\U0001F916 Auto-mode: queuing statuses: %s\n",
      paste(chosen_statuses, collapse = ", ")
    ))
  } else {
    # Interactive mode
    cat("Which file categories would you like to queue for re-download?\n")
    for (i in seq_len(nrow(status_meta))) {
      cat(sprintf("  [%s] %s %s\n",
                  status_meta$code[i], status_meta$icon[i], status_meta$label[i]))
    }
    cat("  [A] All of the above\n")
    cat("  [Q] Quit / download nothing\n\n")

    raw <- readline(prompt = "Enter code(s) separated by commas (e.g. M,H): ")
    raw <- stringr::str_to_upper(stringr::str_trim(raw))

    if (raw == "Q" || raw == "") {
      cat("No files queued. Exiting review.\n")
      return(invisible(audit[0, ]))
    }

    if (raw == "A") {
      chosen_codes <- status_meta$code
    } else {
      chosen_codes <- stringr::str_split(raw, "[,\\s]+")[[1]] %>%
        stringr::str_trim() %>%
        purrr::keep(~ .x %in% status_meta$code)
      bad_codes <- setdiff(
        stringr::str_split(raw, "[,\\s]+")[[1]] %>% stringr::str_trim(),
        c(status_meta$code, "")
      )
      if (length(bad_codes) > 0)
        warning("Unrecognised code(s) ignored: ", paste(bad_codes, collapse = ", "))
    }

    chosen_statuses <- status_meta %>%
      dplyr::filter(code %in% chosen_codes) %>%
      dplyr::pull(status)
  }

  queue <- problems %>% dplyr::filter(status %in% chosen_statuses)

  if (nrow(queue) == 0) {
    cat("No matching files queued.\n")
    return(invisible(audit[0, ]))
  }

  # ---- optionally delete corrupt local files ----
  corrupt_statuses <- c("unreadable", "hash_mismatch")
  to_delete <- queue %>%
    dplyr::filter(status %in% corrupt_statuses, file.exists(local_parquet))

  if (delete_corrupt && nrow(to_delete) > 0) {
    cat(sprintf(
      "\n\U0001F5D1\uFE0F  Deleting %d corrupt/mismatched local parquet file(s) before re-download...\n",
      nrow(to_delete)
    ))
    purrr::walk(to_delete$local_parquet, function(p) {
      fs::file_delete(p)
      cat(sprintf("  Deleted: %s\n", basename(p)))
    })
  }

  # ---- summary ----
  cat(sprintf(
    "\n\U0001F4E5 %d file(s) queued for download:\n",
    nrow(queue)
  ))
  queue %>%
    dplyr::count(status, timeframe) %>%
    dplyr::arrange(status, timeframe) %>%
    print()
  cat("\n")

  invisible(queue)
}


# ===========================================================================
# CONVENIENCE WRAPPER: audit → review → download
# ===========================================================================

#' Full pipeline: build manifest → audit → interactive review → download.
#'
#' Calls `audit_noaa_data()`, then `review_audit()`, then (with user
#' confirmation) `download_noaa_parquet()` for queued files.
#'
#' @inheritParams audit_noaa_data
#' @inheritParams review_audit
#' @param auto_download  If TRUE, proceed to download without a second
#'                       confirmation prompt after the review step.
#'
#' @return Invisibly returns the download results tibble from
#'         `download_noaa_parquet()`, or the queue tibble if download
#'         was declined / queue was empty.
audit_and_download <- function(
    output_path       = "/Users/uqkbairo/MODRRAP/noaa-worries",
    years             = 1985:2025,
    measure           = c("sst", "dhw", "climatology"),
    summary_type      = NULL,
    timeframe         = c("daily", "monthly", "annual", "climatology"),
    verify_hashes     = TRUE,
    ftp_hash_fallback = TRUE,
    auto              = NULL,
    delete_corrupt    = TRUE,
    auto_download     = FALSE
) {
  # Step 1: build manifest + audit
  audit <- audit_noaa_data(
    output_path       = output_path,
    years             = years,
    measure           = measure,
    summary_type      = summary_type,
    timeframe         = timeframe,
    verify_hashes     = verify_hashes,
    ftp_hash_fallback = ftp_hash_fallback
  )

  # Step 2: interactive review
  queue <- review_audit(audit, auto = auto, delete_corrupt = delete_corrupt)

  if (nrow(queue) == 0) return(invisible(queue))

  # Step 3: confirm and download
  if (!auto_download) {
    answer <- readline(prompt = sprintf(
      "Proceed to download %d file(s)? [y/N]: ", nrow(queue)
    ))
    if (!stringr::str_to_lower(stringr::str_trim(answer)) %in% c("y", "yes")) {
      cat("Download cancelled. The queue tibble has been returned invisibly.\n")
      return(invisible(queue))
    }
  }

  # Build the minimal manifest expected by download_noaa_parquet() by
  # repopulating the fields it needs (ftp_url, local paths, etc.)
  # The queue already has all these columns from build_manifest().
  results <- download_noaa_parquet(
    output_path  = output_path,
    years        = years,
    measure      = measure,
    summary_type = summary_type,
    timeframe    = timeframe
    # download_noaa_parquet will skip already-present parquets automatically;
    # the queue tells *us* what needs doing but the skip logic inside the
    # downloader is the authoritative gate.
  )

  invisible(results)
}


# ===========================================================================
# USAGE EXAMPLES
# ===========================================================================

# ---- 1. Just build and inspect the audit tibble ----
# audit <- audit_noaa_data(
#   output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years       = 2020:2025,
#   measure     = "sst",
#   timeframe   = c("daily", "monthly", "annual", "climatology")
# )
# audit %>% dplyr::filter(status != "ok")   # see problems only
# audit %>% dplyr::count(status)            # headline counts

# ---- 2. Review interactively and get a download queue ----
# queue <- review_audit(audit)
# # User is prompted to enter e.g. "M,H" to queue missing + hash-mismatch files

# ---- 3. Full pipeline in one call (interactive) ----
# audit_and_download(
#   output_path = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years       = 2020:2025,
#   measure     = "sst",
#   timeframe   = c("daily", "monthly", "annual", "climatology")
# )

# ---- 4. Non-interactive / scripted mode (auto-queue missing + mismatch) ----
# audit_and_download(
#   output_path   = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years         = 2020:2025,
#   measure       = "dhw",
#   timeframe     = "annual",
#   summary_type  = "max",
#   auto          = c("missing", "hash_mismatch"),
#   auto_download = TRUE
# )

# ---- 5. Existence-only fast scan (skip hash checks) ----
# audit_noaa_data(
#   output_path   = "/Users/uqkbairo/MODRRAP/noaa-worries",
#   years         = 1985:2025,
#   measure       = "sst",
#   timeframe     = "monthly",
#   verify_hashes = FALSE
# )
