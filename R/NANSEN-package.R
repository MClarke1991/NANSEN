#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats formula
#' @importFrom stats na.omit
#' @importFrom stats time
#' @importFrom utils capture.output
#' @importFrom utils combn
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom stats sd
## usethis namespace: end

# Suppress R CMD check warnings for NSE variables used in dplyr/tidyverse functions
utils::globalVariables(c(
  ".", ":=", "a", "abs_diff_per_gene", "across", "activity", "activity_a", "activity_b", "all_of",
  "alt_filename_part", "alt_filename_part.x", "alt_filename_part.y", "approx_csv_row_id",
  "b", "background", "background_neat", "baseline", "bk_command", "bk_filename_part", "bkg_pert",
  "case", "cell_line", "command", "command_arg", "command_arg.x", "command_arg.y",
  "conflict", "conflict_a", "conflict_b", "diff_per_gene", "drug", "drug_name_original",
  "expectation_bma", "expected_result_bma", "experiment_particular", "file_name", "filename",
  "filename_part", "filename_part.x", "filename_part.y", "first", "full_pert", "gene",
  "head", "hi", "id", "id_a", "id_b", "label", "leva", "levb", "level", "lo", "log_file",
  "mean_result", "muta", "mutation", "mutb", "n_blank_input", "n_total", "n_unique", "name",
  "node", "num_exp_perts", "palettes", "pert", "perturbation", "perturbation_bma", "phenotypes",
  "precede", "precedence", "range_from", "range_to", "second", "setNames", "spec_command",
  "type", "value"
))

NULL
