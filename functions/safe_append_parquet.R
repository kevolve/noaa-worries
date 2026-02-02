library(nanoparquet)
library(fs)
# new_df = author_affiliations
# verbose = TRUE

safe_append_parquet <- function(new_df,
                                parquet_file,
                                verbose = FALSE,
                                validate = TRUE,
                                ...) {
  # new_df: a data.frame matching the original file's column and type structure
  # path_to_orig_file: path to existing Parquet author database
  # verbose: whether to print progress messages
  # validate: whether to attempt reading the new file before replacing
  # ...: extra arguments passed to append_parquet()
  
  # If the data does not exist already...
  # dir <- dirname(path_to_orig_file)
  # base <- "author_db-{i}.parquet"
  parquet_folder <- dirname(parquet_file)
  tmp_file <- check_path(file.path(parquet_folder, "tmp", str_replace(basename(parquet_file), "\\.", "_copy.")))
  
  if (!file.exists(parquet_file)) {
    stop(paste0("Parquet file(s) not detected in '", paste0(dirname(parquet_file),"/"),"' folder"))
  }
  
  
  # Reload, append, and resave a copy of database
  ds <- arrow::open_dataset(parquet_file, format = "parquet") %>% 
    collect() %>% # load data
    rbind(., new_df) %>% # append
    dplyr::filter(!duplicated(author_id, fromLast = TRUE)) # avoid duplicates
  
  arrow::write_dataset(
    ds,
    path = dirname(tmp_file),
    partitioning = NULL,
    basename_template = paste0("author_db-{i}_copy.parquet"),
    existing_data_behavior = "overwrite",
    ...
  )
  if (verbose) cat(paste0("Appended copy saved to: '", tmp_file,"'\n"))
  
  ## Old code for if append_parquet worked:
  # if (verbose && file.exists(tmp_file)) cat(paste0("Temp file already exists; overwriting '", basename(tmp_file), "'"))
  # fs::file_copy(parquet_file, tmp_file, overwrite = TRUE)
  # if (verbose) cat("Backed up original to: ", tmp_file)
  # nanoparquet::append_parquet(new_df, file = tmp_file) # currently not working / broken?
  
  # Validation / sanity check before replacing the original
  
  valid <- FALSE
  err_msg <- NULL
  
  # Try reading metadata / row count / a small sample
  try_res <- try({
    
    info <- nanoparquet::read_parquet_info(tmp_file)
    old_info <- nanoparquet::read_parquet_info(parquet_file)
    
    first_author_name <- arrow::read_parquet(tmp_file, as_data_frame = FALSE, n = 4) %>% 
      filter(author_id == "A5060663362") %>%
      select(author_name) %>%
      collect() %>% as.character()
    
    # # Open the dataset (lazy)
    ds <- arrow::open_dataset(tmp_file, format = "parquet")
    ds %>%
      filter(author_id == my_author_id) %>%
      collect()
    
    if (info$num_rows > old_info$num_rows & first_author_name == "E. Kathryn Morris") {
      valid <- TRUE
    } else {
      err_msg <- paste0("Appending failed; new row count (", info$num_rows, 
                        ") not greater than old (", old_info$num_rows, ") and/or data load did not work!")
    }
  }, silent = TRUE)
  
  if (inherits(try_res, "try-error")) {
    # The read / info operation failed
    err_msg <- paste0("Validation read failed: ", conditionMessage(attr(try_res, "condition")))
  }
  
  # If validation not successful, abort
  if (!valid) {
    # Remove temporary file (or leave for manual inspection)
    if (file.exists(tmp_file)) { fs::file_delete(tmp_file) }
    stop("Validation failed, original not replaced.\nError: ", err_msg)
  } else {
    if (verbose) cat("Validation succeeded; proceeding to replace original...")
  }
  
  
  # If we reach here, validation passed (or validation = FALSE)
  fs::file_move(tmp_file, parquet_file)
  if (verbose) cat(paste0("Appending succeeded; replaced original file in: '", dirname(parquet_file),"' \n"))
  
  invisible(TRUE)
  
  cat("Saved! ")
}
