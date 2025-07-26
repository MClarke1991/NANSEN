source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("validate_combo_config handles valid configuration", {
  # Create temporary valid config
  valid_config <- list(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    out_dir = "test_output",
    drug_path = here::here("examples", "combo", "helper_drugs.csv"),
    phenotypes = c("output_a", "output_b"),
    pheno_only = FALSE,
    skip_autopert = TRUE
  )
  
  config_file <- file.path(temp_dir, "valid_combo_config.json")
  jsonlite::write_json(valid_config, config_file, auto_unbox = TRUE)
  
  on.exit(unlink(config_file))
  
  result <- validate_combo_config(config_file)
  
  expect_equal(result$netw_file_path, valid_config$netw_file_path)
  expect_equal(result$backgrounds_path, valid_config$backgrounds_path)
  expect_equal(result$out_dir, valid_config$out_dir)
  expect_equal(result$drug_path, valid_config$drug_path)
  expect_equal(result$phenotypes, valid_config$phenotypes)
  expect_equal(result$pheno_only, FALSE)
  expect_equal(result$skip_autopert, TRUE)
  # Check defaults are applied
  expect_equal(result$use_exclusions, FALSE)
  expect_equal(result$skip_combo_sim, FALSE)
  expect_equal(result$node_col_name, "node")
  expect_equal(result$use_vmcai, TRUE)
  expect_equal(result$pipe_dir, "combo_results")
})

test_that("validate_combo_config applies defaults correctly", {
  # Create minimal config with only required fields
  minimal_config <- list(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "minimal_combo_config.json")
  jsonlite::write_json(minimal_config, config_file, auto_unbox = TRUE)
  
  on.exit(unlink(config_file))
  
  result <- validate_combo_config(config_file)
  
  # Check that defaults are applied
  expect_true(is.na(result$drug_path))
  expect_true(is.na(result$combo_exclusions_path))
  expect_equal(result$project_path, "")
  expect_equal(result$phenotypes, c("output_a", "output_b"))
  expect_equal(result$palettes, list("GnBu", "YlOrRd"))
  expect_equal(result$pheno_only, TRUE)
  expect_equal(result$use_exclusions, FALSE)
  expect_equal(result$skip_autopert, FALSE)
  expect_equal(result$skip_combo_sim, FALSE)
  expect_equal(result$skip_all_pairs, FALSE)
  expect_equal(result$skip_combo_drugs_single, TRUE)
  expect_equal(result$skip_combo_drugs_double, TRUE)
  expect_equal(result$skip_heatmaps, FALSE)
  expect_equal(result$skip_heatmaps_uc, FALSE)
  expect_equal(result$nosat, TRUE)
  expect_equal(result$loserum, FALSE)
  expect_equal(result$node_col_name, "node")
  expect_equal(result$use_vmcai, TRUE)
  expect_equal(result$background_order, c("cancer", "wt"))
  expect_equal(result$w_s_node, 4)
  expect_equal(result$h_s_node, 12.5)
  expect_equal(result$single_fontsize, 12)
  expect_equal(result$node_heat_dir, "node_heatmaps")
  expect_equal(result$drug_conflict_overide, TRUE)
  expect_equal(result$pipe_dir, "combo_results")
  expect_equal(result$log_filename, "PipeLog.log")
})

test_that("validate_combo_config errors on missing config file", {
  expect_snapshot(
    validate_combo_config("nonexistent_config.json"),
    error = TRUE
  )
})

test_that("validate_combo_config errors on invalid JSON", {
  invalid_json_file <- file.path(temp_dir, "invalid_combo.json")
  writeLines("{ invalid json", invalid_json_file)
  
  on.exit(unlink(invalid_json_file))
  
  expect_snapshot(
    validate_combo_config(invalid_json_file),
    error = TRUE
  )
})

test_that("validate_combo_config errors on missing required fields", {
  # Test missing netw_file_path
  config_missing_netw <- list(
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_netw_combo.json")
  jsonlite::write_json(config_missing_netw, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_combo_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing backgrounds_path
  config_missing_bkg <- list(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_bkg_combo.json")
  jsonlite::write_json(config_missing_bkg, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_combo_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing out_dir
  config_missing_out <- list(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv")
  )
  
  config_file <- file.path(temp_dir, "missing_out_combo.json")
  jsonlite::write_json(config_missing_out, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_combo_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
})

test_that("validate_combo_config errors on missing referenced files", {
  # Test missing network file
  config_missing_netw_file <- list(
    netw_file_path = "nonexistent_network.json",
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_netw_file_combo.json")
  jsonlite::write_json(config_missing_netw_file, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_combo_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing backgrounds file
  config_missing_bkg_file <- list(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    backgrounds_path = "nonexistent_backgrounds.csv",
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_bkg_file_combo.json")
  jsonlite::write_json(config_missing_bkg_file, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_combo_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
})