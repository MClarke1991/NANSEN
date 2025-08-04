## Copyright 2022 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

#' Generate results directory for specification check in a standardised way,
#' so that visualisation scripts can generate one that lines up with
#' combo without combo having to pass it on

#' @title get_ap_results_dir
#' @param results_prefix prefix to results directory
#' @param project_path project path for git SHA log, point to git
#'     repo of the network and specification being tested
#' @param out_dir directory where all output files should be stored
#' @param netw_file_path path to network JSON file
#' @return path to results directory, generated to match name of network
#' @export
get_ap_results_dir <- function(results_prefix, project_path, out_dir, netw_file_path) {
    results_dir <- here::here(project_path,
                             out_dir,
                             paste(results_prefix,
                                   stringr::str_remove(
                                                basename(netw_file_path),
                                                ".json"),
                                   sep = "_"))
    results_dir
}
