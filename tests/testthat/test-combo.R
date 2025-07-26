source(here::here("tests", "testthat", "testing_utils.r"))

# Create a directory for test outputs
temp_dir <- here::here("tests/testthat/temp_test_outputs")
# Create a directory for test outputs
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}


bma_path = 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'
test_that("combo integration test - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  # Create temporary directory for test outputs
  out_dir <- file.path(temp_dir, "combo_test_output")

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Run combo function with example data
  expect_no_error(
    suppressWarnings(
      combo(
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
        drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
        out_dir = out_dir,
        bma_path = bma_path,
        log_filename = "Combo.log",
        drug_conflict_overide = TRUE
      )
    )
  )

  # Verify output directory structure
  run_dir <- file.path(out_dir, "COMBO_RUN_helper_combo_1")
  expect_true(dir.exists(run_dir))
  expect_true(dir.exists(file.path(run_dir, "RAW__single__wt")))
  expect_true(dir.exists(file.path(run_dir, "RAW__single__cancer")))
  expect_true(dir.exists(file.path(run_dir, "RAW__double__wt")))
  expect_true(dir.exists(file.path(run_dir, "RAW__double__cancer")))

  # Verify key files exist
  expect_true(file.exists(file.path(run_dir, "parsed_results.csv")))
  expect_true(file.exists(file.path(run_dir, "processed_results.csv")))
  expect_true(file.exists(file.path(run_dir, "conflicts.csv")))

  # Verify CSV structure and content
  parsed_results <- readr::read_csv(file.path(run_dir, "parsed_results.csv"), show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_true(all(c("filename", "time", "id", "lo", "hi", "node", "range_from", "range_to", "formula") %in% colnames(parsed_results)))

  processed_results <- readr::read_csv(file.path(run_dir, "processed_results.csv"), show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_true(all(c("case", "background", "bkg_pert", "muta", "leva", "mutb", "levb", "time", "id", "lo", "hi", "node", "range_from", "range_to", "formula", "mean", "uncertainty") %in% colnames(processed_results)))

  conflicts <- readr::read_csv(file.path(run_dir, "conflicts.csv"), show_col_types = FALSE)
  expect_true(all(c("case", "background", "bkg_pert", "muta", "leva", "mutb", "levb", "conflict_a", "conflict_b", "conflict", "mean_a", "mean_b", "precedence") %in% colnames(conflicts)))

  # Verify JSON files exist in RAW directories
  single_wt_files <- list.files(file.path(run_dir, "RAW__single__wt"), pattern = "\\.json$")
  expect_true(length(single_wt_files) > 0)

  single_cancer_files <- list.files(file.path(run_dir, "RAW__single__cancer"), pattern = "\\.json$")
  expect_true(length(single_cancer_files) > 0)

  double_wt_files <- list.files(file.path(run_dir, "RAW__double__wt"), pattern = "\\.json$")
  expect_true(length(double_wt_files) > 0)

  double_cancer_files <- list.files(file.path(run_dir, "RAW__double__cancer"), pattern = "\\.json$")
  expect_true(length(double_cancer_files) > 0)

  # Verify each JSON file in one directory is valid JSON
  for (json_file in head(single_wt_files, 5)) {  # Test first 5 files to avoid long test times
    json_path <- file.path(run_dir, "RAW__single__wt", json_file)
    expect_no_error(jsonlite::fromJSON(json_path))
  }

  # Snapshot test for processed_results.csv to ensure output doesn't change
  processed_csv_path <- file.path(run_dir, "processed_results.csv")
  processed_data <- readr::read_csv(processed_csv_path, show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_snapshot(processed_data)
})

test_that("combo handles missing network file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  out_dir <- file.path(temp_dir, "combo_error_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    combo(
      netw_file_path = "nonexistent_file.json",
      backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
      out_dir = out_dir,
      log_filename = "Combo.log"
    )
  )
})

test_that("combo handles missing backgrounds file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  out_dir <- file.path(temp_dir, "combo_error_test2")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    combo(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      backgrounds_path = "nonexistent_backgrounds.csv",
      drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
      out_dir = out_dir,
      log_filename = "Combo.log"
    )
  )
})

test_that("combo handles missing drugs file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  out_dir <- file.path(temp_dir, "combo_error_test3")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    combo(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      drug_path = "nonexistent_drugs.csv",
      out_dir = out_dir,
      log_filename = "Combo.log"
    )
  )
})

test_that("combo creates expected directory structure", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  out_dir <- file.path(temp_dir, "combo_structure_test")

  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  suppressWarnings(combo(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
    out_dir = out_dir,
    bma_path = bma_path,
    log_filename = "Combo.log",
    drug_conflict_overide = TRUE
  ))

  # Test the specific directory structure from combo_example_structure.md
  run_dir <- file.path(out_dir, "COMBO_RUN_helper_combo_1")

  # Check main directories
  expect_true(dir.exists(run_dir))
  expect_true(dir.exists(file.path(run_dir, "RAW__single__wt")))
  expect_true(dir.exists(file.path(run_dir, "RAW__single__cancer")))
  expect_true(dir.exists(file.path(run_dir, "RAW__double__wt")))
  expect_true(dir.exists(file.path(run_dir, "RAW__double__cancer")))

  # Check specific files match the documented structure
  expected_files <- c("parsed_results.csv", "processed_results.csv", "conflicts.csv")

  for (file in expected_files) {
    expect_true(file.exists(file.path(run_dir, file)), info = paste("Missing file:", file))
  }

  # Check that each RAW directory contains JSON files
  raw_dirs <- c("RAW__single__wt", "RAW__single__cancer", "RAW__double__wt", "RAW__double__cancer")

  for (raw_dir in raw_dirs) {
    json_files <- list.files(file.path(run_dir, raw_dir), pattern = "\\.json$")
    expect_true(length(json_files) > 0, info = paste("No JSON files in", raw_dir))
  }
})

test_that("combo detects drug conflicts when override is FALSE", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  out_dir <- file.path(temp_dir, "combo_conflict_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Test with conflicting drugs (default helper_combo_drugs_1.csv has conflicts)
  expect_error(
    combo(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
      out_dir = out_dir,
      bma_path = bma_path,
      log_filename = "Combo.log",
      drug_conflict_overide = FALSE
    ),
    "Drug combinations have conflicting effects on the same node"
  )
})

test_that("combo runs successfully with non-conflicting drugs", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA tools")

  out_dir <- file.path(temp_dir, "combo_no_conflict_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Test with non-conflicting drugs
  expect_no_error(
    combo(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      drug_path = here::here("tests", "testthat", "combo", "helper_combo_drugs_no_conflict.csv"),
      out_dir = out_dir,
      bma_path = bma_path,
      log_filename = "Combo.log",
      drug_conflict_overide = FALSE
    )
  )

  # Verify basic output structure exists
  run_dir <- file.path(out_dir, "COMBO_RUN_helper_combo_1")
  expect_true(dir.exists(run_dir))
  expect_true(file.exists(file.path(run_dir, "parsed_results.csv")))
})