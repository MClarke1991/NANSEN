## Copyright 2024 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

##' @title get_all_hashtables
##' @param file_hashtable_dir directory for hashtables
##' @param log_file log filename
##' @return all hashes in a single table
get_all_hashtables <- function(file_hashtable_dir, log_file) {
    ## read all csv in file_hashtable_dir and bind together using tidyverse
    csv_files <- list.files(file_hashtable_dir, pattern = "*.csv", full.names = TRUE)
    
    # Handle empty directory case
    if (length(csv_files) == 0) {
        stop("Expected location for hashtables (", file_hashtable_dir, ") is empty")
    }
    
    all_hashtables <- csv_files %>%
        purrr::map_df(readr::read_csv, .id = "file", lazy = FALSE, show_col_types = FALSE)

    all_unique_perts <- all_hashtables %>%
        dplyr::pull(unhash_full_filename) %>%
        unique()

    all_unique_hash <- all_hashtables %>%
        dplyr::pull(full_filename) %>%
        unique()

    if (length(all_unique_perts) != length(all_unique_hash)) {
        stop("Not all perturbations have been assigned a unique hash, please use the full length filenames.")
    }

    return(all_hashtables)
}
