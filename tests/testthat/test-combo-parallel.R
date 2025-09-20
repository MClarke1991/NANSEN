source(here::here("tests", "testthat", "testing_utils.r"))

# Create a directory for test outputs
temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

bma_path = 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'

test_that("combo_parallel integration test - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  # Create temporary directory for test outputs
  out_dir <- file.path(temp_dir, "combo_parallel_test_output")

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Run combo_parallel function with example data
  expect_no_error(
    suppressWarnings(
      combo_parallel(
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
        n_cores = 2,  # Use 2 cores for testing
        results_prefix = "COMBO_RUN",
        out_dir = out_dir,
        combo_drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
        bma_path = bma_path,
        log_filename = "Combo.log",
        drug_conflict_overide = TRUE
      )
    )
  )

  # Verify per-background directory structure exists
  backgrounds <- c("wt", "cancer")

  for (background in backgrounds) {
    background_dir <- paste(out_dir, background, sep = "_")
    run_dir <- file.path(background_dir, "COMBO_RUN_helper_combo_1")

    expect_true(dir.exists(run_dir), info = paste("Missing run directory for background:", background))
    expect_true(dir.exists(file.path(run_dir, paste0("RAW__single__", background))))
    expect_true(dir.exists(file.path(run_dir, paste0("RAW__double__", background))))

    # Verify key files exist for each background
    expect_true(file.exists(file.path(run_dir, "parsed_results.csv")))
    expect_true(file.exists(file.path(run_dir, "processed_results.csv")))
    expect_true(file.exists(file.path(run_dir, "node_results.csv")))
    expect_true(file.exists(file.path(run_dir, "conflicts.csv")))
  }

  # Verify integrated results files exist
  expected_integrated_files <- c("parsed_integrated_results.csv",
                                 "node_integrated_results.csv",
                                 "processed_integrated_results.csv")

  for (file in expected_integrated_files) {
    expect_true(file.exists(file.path(out_dir, file)), info = paste("Missing integrated file:", file))
  }

  # Verify CSV structure and content for integrated files
  parsed_integrated <- readr::read_csv(file.path(out_dir, "parsed_integrated_results.csv"),
                                       show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_true(all(c("filename", "time", "id", "lo", "hi", "node", "range_from", "range_to", "formula") %in% colnames(parsed_integrated)))

  processed_integrated <- readr::read_csv(file.path(out_dir, "processed_integrated_results.csv"),
                                          show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_true(all(c("case", "background", "bkg_pert", "muta", "leva", "mutb", "levb", "time", "id", "lo", "hi", "node", "range_from", "range_to", "formula", "mean", "uncertainty") %in% colnames(processed_integrated)))

  node_integrated <- readr::read_csv(file.path(out_dir, "node_integrated_results.csv"),
                                     show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_true(all(c("case", "background", "bkg_pert", "muta", "leva", "mutb", "levb", "time", "id", "lo", "hi", "node", "range_from", "range_to", "formula", "mean", "uncertainty") %in% colnames(node_integrated)))

  # Verify that integrated files contain data from both backgrounds
  expect_true("wt" %in% unique(processed_integrated$background))
  expect_true("cancer" %in% unique(processed_integrated$background))

  # Verify JSON files exist in RAW directories for each background
  for (background in backgrounds) {
    background_dir <- paste(out_dir, background, sep = "_")
    run_dir <- file.path(background_dir, "COMBO_RUN_helper_combo_1")

    single_files <- list.files(file.path(run_dir, paste0("RAW__single__", background)), pattern = "\\.json$")
    expect_true(length(single_files) > 0, info = paste("No JSON files in single directory for", background))

    double_files <- list.files(file.path(run_dir, paste0("RAW__double__", background)), pattern = "\\.json$")
    expect_true(length(double_files) > 0, info = paste("No JSON files in double directory for", background))
  }

  # Verify each JSON file in one directory is valid JSON
  first_background_dir <- paste(out_dir, backgrounds[1], sep = "_")
  first_run_dir <- file.path(first_background_dir, "COMBO_RUN_helper_combo_1")
  single_files <- list.files(file.path(first_run_dir, paste0("RAW__single__", backgrounds[1])), pattern = "\\.json$")

  for (json_file in head(single_files, 3)) {  # Test first 3 files to avoid long test times
    json_path <- file.path(first_run_dir, paste0("RAW__single__", backgrounds[1]), json_file)
    expect_no_error(jsonlite::fromJSON(json_path))
  }

  # Snapshot test for integrated processed results to ensure output doesn't change
  expect_snapshot(processed_integrated)
})

test_that("combo_parallel handles missing network file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_error_test1")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    combo_parallel(
      netw_file_path = "nonexistent_file.json",
      combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      out_dir = out_dir,
      log_filename = "Combo.log"
    )
  )
})

test_that("combo_parallel handles missing backgrounds file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_error_test2")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    combo_parallel(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      combo_backgrounds_path = "nonexistent_backgrounds.csv",
      out_dir = out_dir,
      log_filename = "Combo.log"
    )
  )
})

test_that("combo_parallel handles invalid core count", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_error_test3")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Test with invalid core count (should handle gracefully)
  expect_no_error(
    suppressWarnings(
      combo_parallel(
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
        n_cores = 0,  # Invalid core count
        out_dir = out_dir,
        bma_path = bma_path,
        log_filename = "Combo.log"
      )
    )
  )
})

test_that("combo_parallel cleans up temporary files", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_cleanup_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Run parallel combo
  suppressWarnings(
    combo_parallel(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      n_cores = 2,
      out_dir = out_dir,
      bma_path = bma_path,
      log_filename = "Combo.log",
      drug_conflict_overide = TRUE
    )
  )

  # Check that temporary background files are cleaned up
  tmp_files <- list.files(pattern = ".+_tmp_background.csv$")
  expect_true(length(tmp_files) == 0, info = "Temporary background files have not been deleted")
})

test_that("combo_parallel handles single core gracefully", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_single_core_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Test with single core
  expect_no_error(
    suppressWarnings(
      combo_parallel(
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
        n_cores = 1,
        out_dir = out_dir,
        bma_path = bma_path,
        log_filename = "Combo.log",
        drug_conflict_overide = TRUE
      )
    )
  )

  # Verify that results are still created correctly
  expected_integrated_files <- c("parsed_integrated_results.csv",
                                 "node_integrated_results.csv",
                                 "processed_integrated_results.csv")

  for (file in expected_integrated_files) {
    expect_true(file.exists(file.path(out_dir, file)), info = paste("Missing integrated file:", file))
  }
})

test_that("combo_parallel creates expected directory structure", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_structure_test")

  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  suppressWarnings(combo_parallel(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    n_cores = 2,
    out_dir = out_dir,
    bma_path = bma_path,
    log_filename = "Combo.log",
    drug_conflict_overide = TRUE
  ))

  # Test the specific directory structure for parallel combo
  backgrounds <- c("wt", "cancer")

  for (background in backgrounds) {
    background_dir <- paste(out_dir, background, sep = "_")
    run_dir <- file.path(background_dir, "COMBO_RUN_helper_combo_1")

    # Check main directories
    expect_true(dir.exists(run_dir))
    expect_true(dir.exists(file.path(run_dir, paste0("RAW__single__", background))))
    expect_true(dir.exists(file.path(run_dir, paste0("RAW__double__", background))))

    # Check specific files match the documented structure
    expected_files <- c("parsed_results.csv", "processed_results.csv", "conflicts.csv", "node_results.csv")

    for (file in expected_files) {
      expect_true(file.exists(file.path(run_dir, file)), info = paste("Missing file:", file, "for background:", background))
    }

    # Check that each RAW directory contains JSON files
    raw_dirs <- c(paste0("RAW__single__", background), paste0("RAW__double__", background))

    for (raw_dir in raw_dirs) {
      json_files <- list.files(file.path(run_dir, raw_dir), pattern = "\\.json$")
      expect_true(length(json_files) > 0, info = paste("No JSON files in", raw_dir, "for background:", background))
    }
  }

  # Check integrated files at the top level
  expected_integrated_files <- c("parsed_integrated_results.csv",
                                 "node_integrated_results.csv",
                                 "processed_integrated_results.csv")

  for (file in expected_integrated_files) {
    expect_true(file.exists(file.path(out_dir, file)), info = paste("Missing integrated file:", file))
  }
})

test_that("combo_parallel detects drug conflicts when override is FALSE", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  out_dir <- file.path(temp_dir, "combo_parallel_conflict_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Test with conflicting drugs (default helper_combo_drugs_1.csv has conflicts)
  expect_error(
    combo_parallel(
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      combo_backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
      combo_drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
      n_cores = 2,
      out_dir = out_dir,
      bma_path = bma_path,
      log_filename = "Combo.log",
      drug_conflict_overide = FALSE
    ),
    "Drug combinations have conflicting effects on the same node"
  )
})