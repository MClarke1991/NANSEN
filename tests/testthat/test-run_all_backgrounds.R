source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

bma_path = 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'

test_that("run_all_backgrounds works with short_filenames = FALSE - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "run_all_backgrounds requires Windows BMA command line tools")

  # Create temporary directory for test outputs
  out_dir <- file.path(temp_dir, "run_all_backgrounds_test")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Create test data
  background_commands <- data.frame(
    background = "wt",
    bk_command = "",
    bk_filename_part = "growth_factor__1"
  )
  
  perts <- data.frame(
    command_arg = "-ko 1 0",
    filename_part = "a__0",
    alt_filename_part = "node_a__0"
  )

  # Test with short_filenames = FALSE
  expect_no_error({
    run_all_backgrounds(
      background_commands = background_commands,
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      bma_path = bma_path,
      perts = perts,
      combo_type = "single",
      results_dir = out_dir,
      log_file = "test.log",
      precedence = "perturbation",
      short_filenames = FALSE,
      file_hashtable_dir = NULL
    )
  })

  # Verify RAW directory was created
  raw_dir <- file.path(out_dir, "RAW__single__wt")
  expect_true(dir.exists(raw_dir))
  
  # Verify JSON files exist and have normal (unhashed) names
  json_files <- list.files(raw_dir, pattern = "\\.json$")
  expect_true(length(json_files) > 0)
  
  # Verify filenames are NOT hashed (should contain meaningful parts)
  for (json_file in json_files) {
    expect_true(grepl("PERT|__", json_file), 
               info = paste("Expected unhashed filename format, got:", json_file))
  }
})

test_that("run_all_backgrounds works with short_filenames = TRUE - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "run_all_backgrounds requires Windows BMA command line tools")

  # Create temporary directory for test outputs
  out_dir <- file.path(temp_dir, "run_all_backgrounds_hash_test")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  
  hashtable_dir <- file.path(out_dir, "hashtables")
  if (!dir.exists(hashtable_dir)) {
    dir.create(hashtable_dir)
  }

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Create test data
  background_commands <- data.frame(
    background = "wt",
    bk_command = "",
    bk_filename_part = "growth_factor__1"
  )
  
  perts <- data.frame(
    command_arg = "-ko 1 0",
    filename_part = "a__0",
    alt_filename_part = "node_a__0"
  )

  # Test with short_filenames = TRUE
  expect_no_error({
    run_all_backgrounds(
      background_commands = background_commands,
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      bma_path = bma_path,
      perts = perts,
      combo_type = "single",
      results_dir = out_dir,
      log_file = "test.log",
      precedence = "perturbation",
      short_filenames = TRUE,
      file_hashtable_dir = hashtable_dir
    )
  })

  # Verify RAW directory was created
  raw_dir <- file.path(out_dir, "RAW__single__wt")
  expect_true(dir.exists(raw_dir))
  
  # Verify JSON files exist and are hashed
  json_files <- list.files(raw_dir, pattern = "\\.json$")
  expect_true(length(json_files) > 0)
  
  # Verify filenames ARE hashed (32-character MD5 + .json)
  for (json_file in json_files) {
    filename_without_ext <- tools::file_path_sans_ext(json_file)
    expect_true(nchar(filename_without_ext) == 32, 
               info = paste("Expected 32-char hash, got:", nchar(filename_without_ext), "for file:", json_file))
    expect_match(filename_without_ext, "^[a-f0-9]{32}$", 
               info = paste("Expected MD5 hash format for file:", json_file))
  }
  
  # Verify hashtable CSV was created
  hashtable_files <- list.files(hashtable_dir, pattern = "^file_hashtable_single__wt\\.csv$")
  expect_true(length(hashtable_files) == 1)
  
  # Verify hashtable structure
  hashtable <- readr::read_csv(file.path(hashtable_dir, hashtable_files[1]), show_col_types = FALSE)
  expect_true(all(c("unhash_full_filename", "unhash_alt_full_filename", "full_filename", "alt_full_filename") %in% colnames(hashtable)))
  expect_true(nrow(hashtable) > 0)
  
  # Verify hash mapping works correctly
  for (i in seq_len(nrow(hashtable))) {
    # Check that hashed filenames are valid MD5
    hash_full <- tools::file_path_sans_ext(hashtable$full_filename[i])
    hash_alt <- tools::file_path_sans_ext(hashtable$alt_full_filename[i])
    
    expect_true(nchar(hash_full) == 32)
    expect_match(hash_full, "^[a-f0-9]{32}$")
    expect_true(nchar(hash_alt) == 32) 
    expect_match(hash_alt, "^[a-f0-9]{32}$")
    
    # Check that unhashed filenames contain meaningful parts
    expect_true(grepl("PERT|__", hashtable$unhash_full_filename[i]))
    expect_true(grepl("PERT|__", hashtable$unhash_alt_full_filename[i]))
  }
})

test_that("run_all_backgrounds handles multiple backgrounds with hashing - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "run_all_backgrounds requires Windows BMA command line tools")

  # Create temporary directory for test outputs
  out_dir <- file.path(temp_dir, "run_all_backgrounds_multi_test")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  
  hashtable_dir <- file.path(out_dir, "hashtables")
  if (!dir.exists(hashtable_dir)) {
    dir.create(hashtable_dir)
  }

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Create test data with multiple backgrounds
  background_commands <- data.frame(
    background = c("wt", "cancer"),
    bk_command = c("", "-ko 2 1"),
    bk_filename_part = c("growth_factor__1", "growth_factor__1__e__1")
  )
  
  perts <- data.frame(
    command_arg = "-ko 1 0",
    filename_part = "a__0",
    alt_filename_part = "node_a__0"
  )

  # Test with short_filenames = TRUE
  expect_no_error({
    run_all_backgrounds(
      background_commands = background_commands,
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      bma_path = bma_path,
      perts = perts,
      combo_type = "double",
      results_dir = out_dir,
      log_file = "test.log",
      precedence = "perturbation",
      short_filenames = TRUE,
      file_hashtable_dir = hashtable_dir
    )
  })

  # Verify RAW directories were created for both backgrounds
  raw_dir_wt <- file.path(out_dir, "RAW__double__wt")
  raw_dir_cancer <- file.path(out_dir, "RAW__double__cancer")
  expect_true(dir.exists(raw_dir_wt))
  expect_true(dir.exists(raw_dir_cancer))
  
  # Verify hashtable CSV files were created for both backgrounds
  expected_hashtable_files <- c("file_hashtable_double__wt.csv", "file_hashtable_double__cancer.csv")
  actual_hashtable_files <- list.files(hashtable_dir, pattern = "\\.csv$")
  
  for (expected_file in expected_hashtable_files) {
    expect_true(expected_file %in% actual_hashtable_files, 
               info = paste("Missing hashtable file:", expected_file))
  }
})