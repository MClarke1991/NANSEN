library(NANSEN)

## Directory that you can use to keep all results

root_dir <- here::here() # put in manually if this folder is not a git or rproj root
out_dir <- file.path(root_dir, "combo_results", "parallel_combo_results")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
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
exclusions_path <- NA

## Control which things to run
skip_autopert <- TRUE
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
## Filename for log of any errors
log_filename <- "PipeLog.log"
node_col_name <- "node"
combo_log_name <- "Combo.log"
results_prefix <- "COMBO_RUN"

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
  n_cores <- parallel::detectCores()

  combo_parallel(
    netw_file_path = netw_file_path,
    combo_backgrounds_path = combo_backgrounds_path,
    n_cores = n_cores - 1,
    results_prefix = results_prefix,
    out_dir = out_dir,
    project_path = project_path,
    combo_drug_path = combo_drug_path,
    bma_path = bma_path,
    node_col_name = node_col_name,
    use_vmcai = use_vmcai,
    pheno_only = pheno_only,
    phenotypes = phenotypes,
    use_exclusions = use_exclusions,
    exclusions_path = combo_exclusions_path,
    drug_conflict_overide = drug_conflict_overide,
    skip_all_pairs = skip_all_pairs,
    skip_combo_drugs_single = skip_combo_drugs_single,
    skip_combo_drugs_double = skip_combo_drugs_double,
    log_filename = combo_log_name
  )
} else {
  print("Skipping combo")
}

# For visualisation code, see example_combo.r
