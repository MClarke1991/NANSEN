source(here::here("tests", "testthat", "testing_utils.r"))

bma_path = 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'

test_that("debug CSV parsing issues - keep files for analysis", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  # Use a persistent directory that won't be deleted
  debug_dir <- here::here("debug_csv_output")
  if (!dir.exists(debug_dir)) {
    dir.create(debug_dir)
  }
  
  # Set up test logging
  setup_log_file(futile.logger::INFO)
  
  # Don't clean up files - we want to keep them for debugging
  # on.exit({
  #   cleanup_log_file()
  #   if (dir.exists(debug_dir)) {
  #     unlink(debug_dir, recursive = TRUE)
  #   }
  # })

  cat("Running combo to generate CSV files for debugging...\n")
  
  # Run combo function with example data
  expect_no_error(
    suppressWarnings(
      combo(
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
        drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
        out_dir = debug_dir,
        bma_path = bma_path,
        log_filename = "Debug_Combo.log",
        drug_conflict_overide = TRUE
      )
    )
  )

  # Get the run directory
  run_dir <- file.path(debug_dir, "COMBO_RUN_helper_combo_1")
  expect_true(dir.exists(run_dir))
  
  cat("Files generated in:", run_dir, "\n")
  cat("Analyzing CSV parsing issues...\n")

  # Test parsed_results.csv
  parsed_csv_path <- file.path(run_dir, "parsed_results.csv")
  expect_true(file.exists(parsed_csv_path))
  
  cat("Reading parsed_results.csv...\n")
  parsed_data <- readr::read_csv(parsed_csv_path, show_col_types = FALSE)
  parsed_problems <- vroom::problems(parsed_data)
  
  if (nrow(parsed_problems) > 0) {
    cat("PARSED_RESULTS.CSV PROBLEMS:\n")
    print(parsed_problems)
    cat("\n")
  } else {
    cat("No problems found in parsed_results.csv\n")
  }

  # Test processed_results.csv
  processed_csv_path <- file.path(run_dir, "processed_results.csv")
  expect_true(file.exists(processed_csv_path))
  
  cat("Reading processed_results.csv...\n")
  processed_data <- readr::read_csv(processed_csv_path, show_col_types = FALSE)
  processed_problems <- vroom::problems(processed_data)
  
  if (nrow(processed_problems) > 0) {
    cat("PROCESSED_RESULTS.CSV PROBLEMS:\n")
    print(processed_problems)
    cat("\n")
  } else {
    cat("No problems found in processed_results.csv\n")
  }

  # Test conflicts.csv
  conflicts_csv_path <- file.path(run_dir, "conflicts.csv")
  expect_true(file.exists(conflicts_csv_path))
  
  cat("Reading conflicts.csv...\n")
  conflicts_data <- readr::read_csv(conflicts_csv_path, show_col_types = FALSE)
  conflicts_problems <- vroom::problems(conflicts_data)
  
  if (nrow(conflicts_problems) > 0) {
    cat("CONFLICTS.CSV PROBLEMS:\n")
    print(conflicts_problems)
    cat("\n")
  } else {
    cat("No problems found in conflicts.csv\n")
  }
  
  # Save file info for external analysis
  cat("CSV file paths for manual inspection:\n")
  cat("- Parsed results:", parsed_csv_path, "\n")
  cat("- Processed results:", processed_csv_path, "\n") 
  cat("- Conflicts:", conflicts_csv_path, "\n")
  
  cleanup_log_file()
})