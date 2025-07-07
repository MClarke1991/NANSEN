## Copyright 2022 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

library(NANSEN)

## For you to set up
path_to_netw <- file.path("examples", "autopert", "helper_autopert_1.json")
path_to_spec <- file.path(file.path("examples", "autopert", "helper_spec_1.csv"))

## options
out_dir <- file.path("auto_pert_results")
missing_nodes_perturbed_overide <- FALSE
missing_nodes_expected_overide <- FALSE

## Defaults
## note exact format needed. If you have installed using the one-click
## installer on windows this path should not need to be edited
path_to_bma <- 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'

autopert(netw_file_path = path_to_netw,
         spec_path = path_to_spec,
         bma_path = path_to_bma,
         out_dir = out_dir,
         nosat = TRUE,
         loserum = FALSE,
         missing_nodes_perturbed_overide =
             missing_nodes_perturbed_overide,
         missing_nodes_expected_overide =
             missing_nodes_expected_overide
         )
