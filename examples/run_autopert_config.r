#!/usr/bin/env Rscript

## Config-based AutoPert runner script
##
## This script runs the autopert function using parameters specified in a TOML
## configuration file.
##
## Usage: `Rscript examples/run_autopert_config.r path/to/config.toml`
##
## Example: `Rscript examples/run_autopert_config.r examples/autopert_config_example.toml`
##
## Required config fields:
##   - netw_file_path: path to network JSON file
##   - spec_path: path to specification CSV file
##   - out_dir: directory for output files
##
## Optional config fields (with defaults):
##   - bma_path: path to BMA installation
##   - nosat: run without SAT solver (default: true)
##   - loserum: experimental serum option (default: false)
##   - missing_nodes_perturbed_overide: override missing node checks (default: false)
##   - missing_nodes_expected_overide: override missing node checks (default: false)
##   - project_path: git repository path for logging (default: null)
##   - group_vars: variables for grouping experiments (default: ["source", "cell_line", "experiment_particular"])

library(NANSEN)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
    stop("Usage: Rscript run_autopert_config.r <config_file_path>")
}

config_path <- args[1]

cli::cli_alert_info("Loading configuration from:", config_path, "\n")
config <- validate_autopert_config(config_path)

cli::cli_alert_info("Running autopert with configuration:\n")
cli::cli_alert_info("  Network file:", config$netw_file_path, "\n")
cli::cli_alert_info("  Spec file:", config$spec_path, "\n")
cli::cli_alert_info("  Output directory:", config$out_dir, "\n")

autopert(
    netw_file_path = config$netw_file_path,
    spec_path = config$spec_path,
    out_dir = config$out_dir,
    nosat = config$nosat,
    loserum = config$loserum,
    missing_nodes_perturbed_overide = config$missing_nodes_perturbed_overide,
    missing_nodes_expected_overide = config$missing_nodes_expected_overide,
    project_path = config$project_path,
    group_vars = config$group_vars,
    short_filenames = config$short_filenames
)

cli::cli_alert_info("AutoPert run completed.\n")