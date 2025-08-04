## Copyright 2019 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

#' Get BMA network variables
#'
#' Imports the node names, target functions and ranges from a BMA json
#' as a tibble
#'
#' @param netw_file_path File path to a BMA JSON file
#'
#' @return A tibble of the name (name), lower bound of the range (range_from),
#' upper bound of the range (range_to) and Target Function (formula)
#'
#' @export
get_netw_variables <- function(netw_file_path){
  raw_netw_json_details <- jsonlite::fromJSON(netw_file_path)

  netw_variables_init <- raw_netw_json_details$Model$Variables %>%
    janitor::clean_names() %>%
    dplyr::as_tibble()

  if (any(stringr::str_detect(
    dplyr::pull(netw_variables_init, "formula"),
    ("\r|\n")))) {
    message("New line characters in formula of imported JSON will be removed for this analysis (JSON file unchanged).")

    netw_variables <- netw_variables_init %>%
      dplyr::mutate(formula = stringr::str_replace_all(formula, "\r", "")) %>%
      dplyr::mutate(formula = stringr::str_replace_all(formula, "\n", ""))

  } else {
    netw_variables <- netw_variables_init
  }



  netw_variables %>% # check for disallowed characters in node names
    dplyr::pull("name") %>%
      purrr::walk(NANSEN::node_name_check)
  ## check if there a duplicate nodes with the same name
  if (any(duplicated(netw_variables$name))) {
      stop("Node names must be unique")
  }
  ## check if there are nodes with zero granularity.
  zero_gran <- netw_variables %>%
              dplyr::mutate("range_from" = as.numeric(range_from),
                     "range_to" = as.numeric(range_to)) %>%
      dplyr::filter(range_from == range_to)
  if(nrow(zero_gran) > 0) {
      stop(paste("Nodes with zero granularity (max same as min) are often a mistake and cannot be perturbed. Check:",
                    dplyr::pull(zero_gran, "name"),
                    sep = "\n"))
  }
  ## Check if BMA has stored something as a character which should be an int
  if (!all(apply(dplyr::select(netw_variables, -"name", -"formula"), MARGIN = 2,
                 FUN = is.numeric)))
  {

      netw_variables <- netw_variables %>%
          dplyr::mutate(range_from = as.integer(range_from),
                 range_to = as.integer(range_to),
                 id = as.integer(id))
      ## warning("Values in JSON which are expected to be numeric
            ## (Id, RangeFrom, RangeTo) are stored as characters.
            ## Please correct in JSON file.")
  }

  return(netw_variables)
}
