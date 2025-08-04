## Copyright 2023 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

#' Split combination results into node, druggable and drug
#' perturbations and optionally calculate the difference between one
#' positive and one negative characteristic e.g. survival
#'
#' @title split_combo_results
#' @inheritParams combo
#' @return write the results, with survival if enabled, for node,
#'     druggable (only nodes for which there are drugs) and drug
#'     perturbations
#' @export
split_combo_results <- function(results_prefix,
                                project_path,
                                out_dir,
                                netw_file_path,
                                drug_path = NA,
                                node_col_name = "node") {
    results_dir <- get_combo_results_dir(results_prefix = results_prefix,
                          project_path = project_path,
                          out_dir = out_dir,
                          netw_file_path = netw_file_path)

    results_w_drugs <- readr::read_csv(file.path(
                                  results_dir, "processed_results.csv"),
                                  lazy = FALSE,
                                  show_col_types = FALSE)

    netw_variables <- get_netw_variables(netw_file_path) %>%
        dplyr::rename("node" = "name")

    all_nodes <- c(unique(dplyr::pull(netw_variables, node_col_name)), "baseline")
    results_no_drugs <- results_w_drugs %>%
        dplyr::filter(muta %in% all_nodes,
        (mutb %in% all_nodes | is.na(mutb)))
    readr::write_csv(results_no_drugs, file.path(results_dir, "node_results.csv"))

    if (!is.na(drug_path)) {
        drugs <- import_drugs_clean(drug_path = drug_path, show_col_types = FALSE)
        druggable_nodes <- c(unique(dplyr::pull(drugs, node_col_name)), "baseline")
        drug_names <- c(unique(dplyr::pull(drugs, drug)), "baseline")

        results_druggable_broad <- results_w_drugs %>%
            dplyr::filter(muta %in% druggable_nodes,
            (mutb %in% druggable_nodes | is.na(mutb)))

        results_druggable_narrow <- results_w_drugs %>%
            dplyr::filter(muta %in% drug_names,
            (mutb %in% drug_names | is.na(mutb))) %>%
            dplyr::mutate(leva = "", levb = "")
        readr::write_csv(results_druggable_broad, file.path(results_dir, "druggable_results.csv"))
        readr::write_csv(results_druggable_narrow, file.path(results_dir, "drug_results.csv"))
    }
}
