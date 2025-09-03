#!/usr/bin/env Rscript

## Config-based Combo runner script
##
## This script runs the combo function using parameters specified in a TOML
## configuration file instead of hardcoded variables.
##
## Usage: `Rscript examples/run_combo_config.r path/to/config.toml`
##
## Example: `Rscript examples/run_combo_config.r examples/combo_config_example.toml`
##
## Required config fields:
##   - netw_file_path: path to network JSON file
##   - backgrounds_path: path to backgrounds CSV file
##   - out_dir: directory for output files
##
## Optional config fields (with defaults):
##   - drug_path: path to drug perturbations CSV file (default: NA)
##   - combo_exclusions_path: path to exclusions CSV file (default: NA)
##   - project_path: git repository path for logging (default: "")
##   - phenotypes: list of phenotypes to measure (default: ["output_a", "output_b"])
##   - palettes: color palettes for heatmaps (default: ["GnBu", "YlOrRd"])
##   - pheno_only: only process phenotype nodes (default: true)
##   - use_exclusions: exclude some nodes from screening (default: false)
##   - skip_autopert: skip autopert step (default: false)
##   - skip_combo_sim: skip combo simulation (default: false)
##   - skip_all_pairs: skip pairwise combinations (default: false)
##   - skip_combo_drugs_single: skip single drug combinations (default: true)
##   - skip_combo_drugs_double: skip double drug combinations (default: true)
##   - skip_heatmaps: skip heatmap generation (default: false)
##   - skip_heatmaps_uc: skip unclustered heatmaps (default: false)
##   - And many visualization parameters with sensible defaults

library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(NANSEN)
library(here)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
    stop("Usage: Rscript run_combo_config.r <config_file_path>")
}

config_path <- args[1]

cli::cli_alert_info("Loading configuration from:", config_path, "\n")
config <- validate_combo_config(config_path)

cli::cli_alert_info("Running combo with configuration:\n")
cli::cli_alert_info("  Network file:", config$netw_file_path, "\n")
cli::cli_alert_info("  Backgrounds file:", config$backgrounds_path, "\n")
cli::cli_alert_info("  Output directory:", config$out_dir, "\n")

## Directory that you can use to keep all results
pipe_dir <- config$pipe_dir
root_dir <- here() # put in manually if this folder is not a git or rproj root

setwd(root_dir)

if (!dir.exists(pipe_dir)) {
    dir.create(pipe_dir)
}

## Path to BMA install. Note exact format needed. By default is the
## path that the MSI installer from the BMA website uses
bma_path <- 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'

## Name for directory for all results from a run
out_dir <- file.path(pipe_dir, "results")

## Pipeline -----

## autopert
if (!config$skip_autopert) {
    autopert(netw_file_path = config$netw_file_path,
             spec_path = config$spec_path,
             bma_path = bma_path,
             out_dir = out_dir,
             nosat = config$nosat,
             loserum = config$loserum,
             missing_nodes_perturbed_overide = config$missing_nodes_perturbed_overide,
             missing_nodes_expected_overide = config$missing_nodes_expected_overide,
             project_path = config$project_path,
             short_filenames = config$short_filenames
             )
} else {
    print("Skipping specification testing")
}

## combo
if (!config$skip_combo_sim) {
    combo(netw_file_path = config$netw_file_path,
          backgrounds_path = config$backgrounds_path,
          drug_path = config$drug_path,
          out_dir = out_dir,
          project_path = config$project_path,
          node_col_name = config$node_col_name,
          use_vmcai = config$use_vmcai,
          pheno_only = config$pheno_only,
          phenotypes = config$phenotypes,
          use_exclusions = config$use_exclusions,
          exclusions_path = config$combo_exclusions_path,
          log_filename = config$log_filename,
          drug_conflict_overide = config$drug_conflict_overide,
          skip_all_pairs = config$skip_all_pairs,
          skip_drugs_single = config$skip_combo_drugs_single,
          skip_drugs_pairs = config$skip_combo_drugs_double,
          short_filenames = config$short_filenames
          )
} else {
    print("Skipping combo")
}

## Post-processing ------------

split_combo_results(
    results_prefix = "COMBO_RUN",
    project_path = config$project_path,
    out_dir = out_dir,
    netw_file_path = config$netw_file_path,
    drug_path = config$drug_path,
    node_col_name = config$node_col_name
)

## Visualisation ------------

if (!config$skip_heatmaps) {
    plot_heatmaps(
        results_file = "node_results.csv",
        results_prefix = "COMBO_RUN",
        project_path = config$project_path,
        out_dir = out_dir,
        netw_file_path = config$netw_file_path,
        vis_dir = config$node_heat_dir,
        type = "node",
        single_fontsize = config$single_fontsize,
        single_druggable_fontsize = config$single_druggable_fontsize,
        single_drugs_fontsize = config$single_drugs_fontsize,
        double_fontsize = config$double_fontsize,
        double_druggable_font_size = config$double_druggable_font_size,
        double_drugs_font_size = config$double_drugs_font_size,
        background_order = config$background_order,
        w_s = config$w_s_node,
        h_s = config$h_s_node,
        w_d = config$w_d_node,
        h_d = config$h_d_node
    )
    if (!is.na(config$drug_path)) {
        plot_heatmaps(
            results_file = "druggable_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = config$project_path,
            out_dir = out_dir,
            netw_file_path = config$netw_file_path,
            vis_dir = config$druggable_heat_dir,
            type = "druggable",
            single_fontsize = config$single_fontsize,
            single_druggable_fontsize = config$single_druggable_fontsize,
            single_drugs_fontsize = config$single_drugs_fontsize,
            double_fontsize = config$double_fontsize,
            double_druggable_font_size = config$double_druggable_font_size,
            double_drugs_font_size = config$double_drugs_font_size,
            background_order = config$background_order,
            w_s = config$w_s_druggable,
            h_s = config$h_s_druggable,
            w_d = config$w_d_druggable,
            h_d = config$h_d_druggable
        )
    }

    if (!is.na(config$drug_path)) {
        plot_heatmaps(
            results_file = "drug_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = config$project_path,
            out_dir = out_dir,
            netw_file_path = config$netw_file_path,
            vis_dir = config$drug_heat_dir,
            type = "drug",
            single_fontsize = config$single_fontsize,
            single_druggable_fontsize = config$single_druggable_fontsize,
            single_drugs_fontsize = config$single_drugs_fontsize,
            double_fontsize = config$double_fontsize,
            double_druggable_font_size = config$double_druggable_font_size,
            double_drugs_font_size = config$double_drugs_font_size,
            background_order = config$background_order,
            w_s = config$w_s_drug,
            h_s = config$h_s_drug,
            w_d = config$w_d_drug,
            h_d = config$h_d_drug
        )
    }
}

if (!config$skip_heatmaps_uc) {
    plot_heatmaps(
        results_file = "node_results.csv",
        results_prefix = "COMBO_RUN",
        project_path = config$project_path,
        out_dir = out_dir,
        netw_file_path = config$netw_file_path,
        vis_dir = config$node_heat_dir_uc,
        type = "node",
        cluster_rows = FALSE,
        cluster_double_cols = FALSE,
        single_fontsize = config$single_fontsize,
        single_druggable_fontsize = config$single_druggable_fontsize,
        single_drugs_fontsize = config$single_drugs_fontsize,
        double_fontsize = config$double_fontsize,
        double_druggable_font_size = config$double_druggable_font_size,
        double_drugs_font_size = config$double_drugs_font_size,
        background_order = config$background_order,
        w_s = config$w_s_node,
        h_s = config$h_s_node,
        w_d = config$w_d_node,
        h_d = config$h_d_node
    )

    if (!is.na(config$drug_path)) {
        plot_heatmaps(
            results_file = "druggable_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = config$project_path,
            out_dir = out_dir,
            netw_file_path = config$netw_file_path,
            vis_dir = config$druggable_heat_dir_uc,
            type = "druggable",
            cluster_rows = FALSE,
            cluster_double_cols = FALSE,
            single_fontsize = config$single_fontsize,
            single_druggable_fontsize = config$single_druggable_fontsize,
            single_drugs_fontsize = config$single_drugs_fontsize,
            double_fontsize = config$double_fontsize,
            double_druggable_font_size = config$double_druggable_font_size,
            double_drugs_font_size = config$double_drugs_font_size,
             background_order = config$background_order,
             w_s = config$w_s_druggable,
             h_s = config$h_s_druggable,
             w_d = config$w_d_druggable,
             h_d = config$h_d_druggable
        )
    }

    if (!is.na(config$drug_path)) {
        plot_heatmaps(
            results_file = "drug_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = config$project_path,
            out_dir = out_dir,
            netw_file_path = config$netw_file_path,
            vis_dir = config$drug_heat_dir_uc,
            type = "drug",
            cluster_rows = FALSE,
            cluster_double_cols = FALSE,
            single_fontsize = config$single_fontsize,
            single_druggable_fontsize = config$single_druggable_fontsize,
            single_drugs_fontsize = config$single_drugs_fontsize,
            double_fontsize = config$double_fontsize,
            double_druggable_font_size = config$double_druggable_font_size,
            double_drugs_font_size = config$double_drugs_font_size,
             background_order = config$background_order,
             w_s = config$w_s_drug,
             h_s = config$h_s_drug,
             w_d = config$w_d_drug,
             h_d = config$h_d_drug
        )
    }
}

cli::cli_alert_info("Combo run completed.\n")