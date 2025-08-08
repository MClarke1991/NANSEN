source(here::here("tests", "testthat", "testing_utils.r"))

# Create a directory for test outputs
temp_dir <- here::here("tests/testthat/temp_test_outputs")
# Create a directory for test outputs
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}


bma_path = 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'
test_that("combo integration test - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

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

test_that("combo integration test with short filenames - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  # Create temporary directory for test outputs
  out_dir <- file.path(temp_dir, "combo_short_filenames_test")

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Run combo function with short_filenames = TRUE
  expect_no_error(
    suppressWarnings(
      combo(
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
        drug_path = NA,
        bma_path = bma_path,
        results_prefix = "COMBO_RUN",
        out_dir = out_dir,
        project_path = "",
        node_col_name = "node",
        use_vmcai = TRUE,
        pheno_only = TRUE,
        phenotypes = c("output_a", "output_b"),
        use_exclusions = FALSE,
        exclusions_path = NA,
        drug_conflict_overide = FALSE,
        skip_drugs_single = TRUE,
        skip_drugs_pairs = TRUE,
        skip_all_pairs = FALSE,
        short_filenames = TRUE,  # Test with hashing enabled
        log_filename = "Combo.log"
      )
    )
  )

  # Verify basic output structure exists
  run_dir <- file.path(out_dir, "COMBO_RUN_helper_combo_1")
  expect_true(dir.exists(run_dir))
  expect_true(file.exists(file.path(run_dir, "parsed_results.csv")))
  expect_true(file.exists(file.path(run_dir, "processed_results.csv")))
  
  # Verify file_hashtables directory was created
  hashtable_dir <- file.path(run_dir, "file_hashtables")
  expect_true(dir.exists(hashtable_dir))
  
  # Verify hashtable CSV files were created
  hashtable_files <- list.files(hashtable_dir, pattern = "^file_hashtable_.*\.csv$")
  expect_true(length(hashtable_files) > 0)
  
  # Check hashtable structure
  for (hashtable_file in hashtable_files) {
    hashtable <- readr::read_csv(file.path(hashtable_dir, hashtable_file), show_col_types = FALSE)
    expect_true(all(c("unhash_full_filename", "unhash_alt_full_filename", "full_filename", "alt_full_filename") %in% colnames(hashtable)))
    
    # Verify hash format (32-char MD5)
    for (hash in hashtable$full_filename) {
      hash_without_ext <- tools::file_path_sans_ext(hash)
      expect_true(nchar(hash_without_ext) == 32, 
                 info = paste("Expected 32-char hash, got:", nchar(hash_without_ext)))
      expect_match(hash_without_ext, "^[a-f0-9]{32}$", 
                 info = paste("Expected MD5 hash format for:", hash))
    }
  }
  
  # Verify RAW output directories contain hashed files
  raw_dirs <- list.dirs(run_dir, pattern = "RAW__", recursive = FALSE)
  expect_true(length(raw_dirs) > 0)
  
  for (raw_dir in raw_dirs) {
    json_files <- list.files(raw_dir, pattern = "\.json$")
    if (length(json_files) > 0) {
      # Check that filenames are hashed
      for (json_file in json_files) {
        filename_without_ext <- tools::file_path_sans_ext(json_file)
        expect_true(nchar(filename_without_ext) == 32, 
                   info = paste("Expected 32-char hash in RAW dir, got:", nchar(filename_without_ext), "for file:", json_file))
        expect_match(filename_without_ext, "^[a-f0-9]{32}$", 
                   info = paste("Expected MD5 hash format in RAW dir for file:", json_file))
      }
    }
  }
  
  # Verify processed results still work correctly with hashed filenames
  processed_results <- readr::read_csv(file.path(run_dir, "processed_results.csv"), show_col_types = FALSE)
  expect_true(nrow(processed_results) > 0)
  expect_true(all(c("name", "lo", "hi") %in% colnames(processed_results)))
  
  # Verify parsed results contain properly mapped filenames
  parsed_results <- readr::read_csv(file.path(run_dir, "parsed_results.csv"), show_col_types = FALSE, col_types = readr::cols(formula = "c"))
  expect_true(nrow(parsed_results) > 0)
  expect_true(all(c("filename", "time", "id", "lo", "hi", "name") %in% colnames(parsed_results)))
  
  # The parsed results should contain the original (unhashed) filenames due to mapping
  # This verifies that get_all_hashtables() worked correctly
  for (filename in unique(parsed_results$filename)) {
    # Should contain meaningful parts like "PERT" rather than being just a hash
    expect_true(grepl("PERT|__", filename), 
               info = paste("Expected unhashed filename format, got:", filename))
  }
})
