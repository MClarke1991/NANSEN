## Copyright 2019 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

#' Get attractor from BioCheck VMCAI results json file
#'
#' Finds the values to which the range of all nodes can be restricted by the
#' VMCAI algorithm of BMA BioCheck, at the last timepoint outputted.
#'
#' @param filepath path to a BioCheck JSON results file
#'
#' @return dataframe of:
#' "time" time steps taken to restrict values as much as possible;
#' "id" id number of node;
#' "lo" lower bound to which node value was restricted;
#' "hi" upper bound to which node value was restricted
#'
#' @export
parse_biocheck_json <- function(filepath){
  df <- jsonlite::fromJSON(filepath) %>%
    magrittr::extract2("Ticks") %>% # see https://stackoverflow.com/a/27100797/10923234
    dplyr::filter(.data$Time == max(.data$Time)) %>%
    tidyr::unnest(cols = c("Variables"))
  return(df)
}

#' Get attractors from BioCheck VMCAI results json files in directory
#'
#' Finds the values to which the range of all nodes can be restricted by the
#' VMCAI algorithm of BMA BioCheck, at the last timepoint outputted, and stores
#' as one dataframe, using the filename as a reference column
#' Also makes more human readable by getting the relevant information
#' per node from the network_variables.
#' Ignores cex files.
#'
#' @title parse_biocheck_dir
#' @param dir path to a directory containing BioCheck JSON results files
#' @param netw_variables dataframe of BMA JSON variables, created with
#'  `get_netw_variables`
#' @param rec boolean to specify whether to apply recursively. If TRUE, stores
#' directory structure in "filename" column of output.
#'
#' @return dataframe of:
#'
#' "filename" filename of the json from which the results come (path from dir to
#' file if `rec = TRUE`)
#'
#' "time" time steps taken to restrict values as much as possible;
#'
#' "id" id number of node;
#'
#' "lo" lower bound to which node value was restricted;
#'
#' "hi" upper bound to which node value was restricted
#' "name" node name
#'
#' "range_from" lower bound of unrestricted node range
#'
#' "range_to" upper bound of unrestricted node range
#'
#' "formula" node target function
#'
#'
#' @export
parse_biocheck_dir <- function(dir, netw_variables, rec = FALSE) {
  files <- dir(dir, pattern = "\\.json$", recursive = rec) %>%
    purrr::discard(~stringr::str_detect(.x, "_cex.json")) # Remove CEX files

  if (length(files) == 0) {
    stop("No JSON files found in the specified directory.")
  }

  pb <- progress::progress_bar$new(total = length(files),
                                   force = TRUE,
                                   clear = FALSE,
                                   format =
                                       " [:bar] :percent eta: :eta elapsed: :elapsedfull"
                                   )
                                        # Initialise progress bar
  data <- tibble::tibble("filename" = files) %>% # create a data frame
    # holding the file names
    dplyr::mutate(file_contents =
                    purrr::map(.data$filename,
                               .f = function(x) {
                                   pb$tick()
                                   parse_biocheck_json(file.path(dir, x))
                                 # Note that covr throws error here, but I think
                                 # new version of rlang is breaking it
                                 # see https://github.com/r-lib/covr/issues/377
                               }
                    )
    ) %>%
    tidyr::unnest(cols = c("file_contents")) %>%
    janitor::clean_names() %>%
    dplyr::left_join(netw_variables, by = "id") # get human readable gene names
  return(data)
}

##' @title parse_biocheck_dir_apend
##' @export
##' @param existing_file filename of existing file to apend to
##' @param dir directory of results JSON files
##' @param netw_variables network variables from `get_netw_variables`
##' @param rec apply recursively over a directory over directories
parse_biocheck_dir_apend <- function(existing_file, dir, netw_variables, rec = FALSE) {
    files_all <- dir(dir, pattern = "\\.json$", recursive = rec) %>%
        purrr::discard(~stringr::str_detect(.x, "_cex.json")) # Remove CEX files

    existing_results <- readr::read_csv(existing_file, col_types = readr::cols(formula = "c"),
                                        lazy = FALSE) %>%
        dplyr::mutate(formula = tidyr::replace_na(formula, ""))
    files_done <- dplyr::pull(existing_results, filename) %>%
        unique()

    files_todo <- setdiff(files_all, files_done)

    if (length(files_todo != 0)) {
        print("Appending new files.")
        print(files_todo)
        pb <- progress::progress_bar$new(total = length(files_todo),
                                         force = TRUE,
                                         clear = FALSE,
                                         format =
                                             " [:bar] :percent eta: :eta elapsed: :elapsedfull"
                                         )
                                        # Initialise progress bar
        data <- tibble::tibble("filename" = files_todo) %>% # create a data frame
                                        # holding the file names
            dplyr::mutate(file_contents =
                              purrr::map(.data$filename,
                                         .f = function(x) {
                                             pb$tick()
                                             parse_biocheck_json(file.path(dir, x))
                                        # Note that covr throws error here, but I think
                                        # new version of rlang is breaking it
                                        # see https://github.com/r-lib/covr/issues/377
                                         }
                                         )
                          ) %>%
            tidyr::unnest(cols = c("file_contents")) %>%
            janitor::clean_names() %>%
            dplyr::left_join(netw_variables, by = "id") # get human readable gene names

        parsed <- dplyr::bind_rows(existing_results, data)
    } else {
        print("All files already parsed.")
        parsed <- existing_results
    }
    return(parsed)
}
