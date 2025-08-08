## Copyright 2024 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

##' @title get_all_hashtables
##' @param file_hashtable_dir directory for hashtables
##' @param log_file log filename
##' @return all hashes in a single table
##' @author Matthew A. Clarke \email{matthewaclarke1991@gmail.com}
get_all_hashtables <- function(file_hashtable_dir, log_file) {
    ## read all csv in file_hashtable_dir and bind together using tidyverse
    all_hashtables <- list.files(file_hashtable_dir, pattern = "*.csv", full.names = TRUE) %>%
        map_df(read_csv, .id = "file", lazy = FALSE, show_col_types = FALSE)

    all_unique_perts <- all_hashtables %>%
        pull(unhash_full_filename) %>%
        unique()

    all_unique_hash <- all_hashtables %>%
        pull(full_filename) %>%
        unique()

    if (length(all_unique_perts) != length(all_unique_hash)) {
        log_danger(text = "Not all perturbations have been assigned a unique hash, please use the full length filenames.",
                   name = log_file)
    }

    return(all_hashtables)
}
