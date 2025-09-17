library(conflicted)
library(tidyverse)
library(NANSEN)
library(magrittr)
library(here)
library(foreach)
library(doParallel)
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

## Options to leave alone (deprecated) ------

nosat <- TRUE
loserum <- FALSE
node_col_name <- "node"
use_vmcai <- TRUE

## Initialisation ---------

## Path to BMA install. Note exact format needed. By default is the
## path that the MSI installer from teh BMA website uses
bma_path <- 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'

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
  backgrounds = read_csv(combo_backgrounds_path)
  background_list = unique(backgrounds$background)
  
  n_cores = detectCores()
  registerDoParallel(n_cores - 1)
  foreach(current_background = background_list,
          .packages = c("NANSEN", "readr", "here", "dplyr")) %dopar% {
            
            background_tmp_path = paste0(current_background, "_tmp_background.csv")
            
            backgrounds %>%
              filter(background == current_background) %>%
              write_csv(background_tmp_path)
            
            on.exit(if (file.exists(background_tmp_path)) file.remove(background_tmp_path))
            
            current_out_dir = paste(out_dir, current_background, sep = "_")
            
            combo(netw_file_path = netw_file_path,
                  backgrounds_path = background_tmp_path,
                  drug_path = combo_drug_path,
                  out_dir = current_out_dir,
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
            
            ### Post-processing
            split_combo_results(
              results_prefix = "COMBO_RUN",
              project_path = project_path,
              out_dir = current_out_dir,
              netw_file_path = netw_file_path,
              drug_path = combo_drug_path, 
              node_col_name = node_col_name
            )
          }
  
  ### Integrate results
  parsed_results = list.dirs(pipe_dir, recursive = FALSE) %>%
    map(\(x) list.dirs(x, recursive = FALSE)) %>%
    list_flatten() %>%
    map(\(x) read_csv(paste0(x, "/parsed_results.csv"))) %>%
    list_rbind() %T>%
    write_csv(paste0(pipe_dir, "/parsed_integrated_results.csv"))

  node_results = list.dirs(pipe_dir, recursive = FALSE) %>%
    map(\(x) list.dirs(x, recursive = FALSE)) %>%
    list_flatten() %>%
    map(\(x) read_csv(paste0(x, "/node_results.csv"))) %>%
    list_rbind() %T>%
    write_csv(paste0(pipe_dir, "/node_integrated_results.csv"))
  
  processed_results = list.dirs(pipe_dir, recursive = FALSE) %>%
    map(\(x) list.dirs(x, recursive = FALSE)) %>%
    list_flatten() %>%
    map(\(x) read_csv(paste0(x, "/processed_results.csv"))) %>%
    list_rbind() %T>%
    write_csv(paste0(pipe_dir, "/processed_integrated_results.csv"))
  
} else {
  print("Skipping combo")
}

# For visualisation code, see example_combo.r
