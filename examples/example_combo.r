## Copyright 2023 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

## Example of combination script
## Created 2023-02-23


library(conflicted)
library(tidyverse)
library(NANSEN)
library(here)
conflict_prefer("pull", "dplyr")
conflict_prefer("filter", "dplyr")

## Directory that you can use to keep all results
pipe_dir <- "combo_results"
root_dir <- here() # put in manually if this folder is not a git or rproj root

setwd(root_dir)

if (!dir.exists(pipe_dir)) {
    dir.create(pipe_dir)
}

## USER TO ADJUST -------------------

## Paths -------------

## Note that the `netw_file_path` must be RELATIVE to the working
## directory, not an absolute path, due to how BMA command line
## works. All other paths can be absolute or relative.
netw_file_path <- file.path("examples", "combo", "helper_combo_1.json")
spec_path <- file.path("examples", "autopert", "helper_spec_2.csv")
combo_backgrounds_path <- file.path("examples", "combo", "helper_combo_bkg_1.csv")
combo_drug_path <- NA

## Optional: list of nodes not to perturb, so as to speed up computation
combo_exclusions_path <- file.path("tests", "testthat", "combo", "helper_combo_exclude.csv")
## Optional: path for root directory of the project
project_path <- ""

## Settings -----------

## Phenotypes to measure
phenotypes <- c("output_a", "output_b")

## Options to change---------

## Colour palettes for heatmap, must be the same as number of phenotypes measured
palettes <- list("GnBu", "YlOrRd") # expects colours listed in  `RColorBrewer::display.brewer.all()`

## Autopert

## override check for all experiments containing a perturbation
missing_nodes_perturbed_overide <- FALSE
## override check for all experiments containing a measurement
missing_nodes_expected_overide <- FALSE

## Combo settings

## Only add `phenotypes` to processed results file, speeds up
## processing and makes for a lighter file
pheno_only <- TRUE

## Exclude some nodes from mono and combo screen for faster results
use_exclusions <- FALSE

## Control which things to run
skip_autopert <- FALSE
skip_combo_sim <- FALSE
skip_all_pairs <- FALSE ## skip pairwise combinations of nodes
skip_combo_drugs_single <- TRUE
skip_combo_drugs_double <- TRUE

## Control which heatmaps to plot
skip_heatmaps <- FALSE
skip_heatmaps_uc <- FALSE #unclustered

## Options to leave alone (deprecated) ------

nosat <- TRUE
loserum <- FALSE
node_col_name <- "node"
use_vmcai <- TRUE

## Visualisation Options ------

## Heatmaps

background_order <- c("cancer", "wt")

## Single nodes
w_s_node <- 4
h_s_node <- 12.5
single_fontsize <- 12

## Single druggable
w_s_druggable <- 4
h_s_druggable <- 7
single_druggable_fontsize <- 10

## Single drug
w_s_drug <- 4
h_s_drug <- 7
single_drugs_fontsize <- 5

## Double node
w_d_node <- 12.5
h_d_node <- 12.5
double_fontsize <- 10

## Double druggable
w_d_druggable <- 12.5
h_d_druggable <- 12.5
double_druggable_font_size <- 14

## Double drugs
w_d_drug <- 12.5
h_d_drug <- 12.5
double_drugs_font_size <- 14

## Visualisation directories

node_heat_dir <- "node_heatmaps"
druggable_heat_dir <- "druggable_heatmaps"
drug_heat_dir <- "drug_heatmaps"
node_heat_dir_uc <- "unclust_node_heatmaps"
druggable_heat_dir_uc <- "unclust_druggable_heatmaps"
drug_heat_dir_uc <- "unclust_drug_heatmaps"

## Initialisation ---------

## Path to BMA install. Note exact format needed. By default is the
## path that the MSI installer from teh BMA website uses
bma_path <- 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'
## BMA tools path (undefined variable fix)
## Allow drug conflicts override
drug_conflict_overide <- TRUE
## Name for directory for all results from a run
out_dir <- file.path(pipe_dir, "results")
## Filename for log of any errors
log_filename <- "PipeLog.log"

## Pipeline -----

## autopert
if (!skip_autopert) {
    autopert(netw_file_path = netw_file_path,
             spec_path = spec_path,
             bma_path = bma_path,
             out_dir = out_dir,
             nosat = nosat,
             loserum = loserum,
             missing_nodes_perturbed_overide =
                 missing_nodes_perturbed_overide,
             missing_nodes_expected_overide =
                 missing_nodes_expected_overide,
             project_path = project_path,
             )
} else {
    print("Skipping specification testing")
}

## combo
if (!skip_combo_sim) {
    combo(netw_file_path = netw_file_path,
          backgrounds_path = combo_backgrounds_path,
          drug_path = combo_drug_path,
          out_dir = out_dir,
          project_path = project_path,
          node_col_name = "node",
          use_vmcai = use_vmcai,
          pheno_only = pheno_only,
          phenotypes = phenotypes,
          use_exclusions = use_exclusions,
          exclusions_path = NA,
          log_filename = "Combo.log", 
          drug_conflict_overide = drug_conflict_overide,
          skip_all_pairs = skip_all_pairs,
          skip_drugs_single = skip_combo_drugs_single,
          skip_drugs_pairs = skip_combo_drugs_double
          )
} else {
    print("Skipping combo")
}

## Post-processing ------------

split_combo_results(
    results_prefix = "COMBO_RUN",
    project_path = project_path,
    out_dir = out_dir,
    netw_file_path = netw_file_path,
    drug_path = combo_drug_path, 
    node_col_name = node_col_name
)

## Visualisation ------------

if (!skip_heatmaps) {
    plot_heatmaps(
        results_file = "node_results.csv",
        results_prefix = "COMBO_RUN",
        project_path = project_path,
        out_dir = out_dir,
        netw_file_path = netw_file_path,
        vis_dir = node_heat_dir,
        type = "node",
        single_fontsize = single_fontsize,
        single_druggable_fontsize = single_druggable_fontsize,
        single_drugs_fontsize = single_drugs_fontsize,
        double_fontsize = double_fontsize,
        double_druggable_font_size = double_druggable_font_size,
        double_drugs_font_size = double_drugs_font_size,
        background_order = background_order,
        w_s = w_s_node,
        h_s = h_s_node,
        w_d = w_d_node,
        h_d = h_d_node
    )
    if (!is.na(combo_drug_path)) {
        plot_heatmaps(
            results_file = "druggable_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = project_path,
            out_dir = out_dir,
            netw_file_path = netw_file_path,
            vis_dir = druggable_heat_dir,
            type = "druggable",
            single_fontsize = single_fontsize,
            single_druggable_fontsize = single_druggable_fontsize,
            single_drugs_fontsize = single_drugs_fontsize,
            double_fontsize = double_fontsize,
            double_druggable_font_size = double_druggable_font_size,
            double_drugs_font_size = double_drugs_font_size,
            background_order = background_order,
            w_s = w_s_druggable,
            h_s = h_s_druggable,
            w_d = w_d_druggable,
            h_d = h_d_druggable
        )
    }

    if (!is.na(combo_drug_path)) {
        plot_heatmaps(
            results_file = "drug_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = project_path,
            out_dir = out_dir,
            netw_file_path = netw_file_path,
            vis_dir = drug_heat_dir,
            type = "drug",
            single_fontsize = single_fontsize,
            single_druggable_fontsize = single_druggable_fontsize,
            single_drugs_fontsize = single_drugs_fontsize,
            double_fontsize = double_fontsize,
            double_druggable_font_size = double_druggable_font_size,
            double_drugs_font_size = double_drugs_font_size,
            background_order = background_order,
            w_s = w_s_drug,
            h_s = h_s_drug,
            w_d = w_d_drug,
            h_d = h_d_drug
        )
    }
}

if (!skip_heatmaps_uc) {
    plot_heatmaps(
        results_file = "node_results.csv",
        results_prefix = "COMBO_RUN",
        project_path = project_path,
        out_dir = out_dir,
        netw_file_path = netw_file_path,
        vis_dir = node_heat_dir_uc,
        type = "node",
        cluster_rows = FALSE,
        cluster_double_cols = FALSE,
        single_fontsize = single_fontsize,
        single_druggable_fontsize = single_druggable_fontsize,
        single_drugs_fontsize = single_drugs_fontsize,
        double_fontsize = double_fontsize,
        double_druggable_font_size = double_druggable_font_size,
        double_drugs_font_size = double_drugs_font_size,
        background_order = background_order,
        w_s = w_s_node,
        h_s = h_s_node,
        w_d = w_d_node,
        h_d = h_d_node
    )

    if (!is.na(combo_drug_path)) {
        plot_heatmaps(
            results_file = "druggable_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = project_path,
            out_dir = out_dir,
            netw_file_path = netw_file_path,
            vis_dir = druggable_heat_dir_uc,
            type = "druggable",
            cluster_rows = FALSE,
            cluster_double_cols = FALSE,
            single_fontsize = single_fontsize,
            single_druggable_fontsize = single_druggable_fontsize,
            single_drugs_fontsize = single_drugs_fontsize,
            double_fontsize = double_fontsize,
            double_druggable_font_size = double_druggable_font_size,
            double_drugs_font_size = double_drugs_font_size,
             background_order = background_order,
             w_s = w_s_druggable,
             h_s = h_s_druggable,
             w_d = w_d_druggable,
             h_d = h_d_druggable
        )
    }

    if (!is.na(combo_drug_path)) {
        plot_heatmaps(
            results_file = "drug_results.csv",
            results_prefix = "COMBO_RUN",
            project_path = project_path,
            out_dir = out_dir,
            netw_file_path = netw_file_path,
            vis_dir = drug_heat_dir_uc,
            type = "drug",
            cluster_rows = FALSE,
            cluster_double_cols = FALSE,
            single_fontsize = single_fontsize,
            single_druggable_fontsize = single_druggable_fontsize,
            single_drugs_fontsize = single_drugs_fontsize,
            double_fontsize = double_fontsize,
            double_druggable_font_size = double_druggable_font_size,
            double_drugs_font_size = double_drugs_font_size,
             background_order = background_order,
             w_s = w_s_drug,
             h_s = h_s_drug,
             w_d = w_d_drug,
             h_d = h_d_drug
        )
    }
}
