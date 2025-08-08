source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("validate_autopert_config handles valid configuration", {
  # Create temporary valid config
  valid_config <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output",
    nosat = FALSE,
    loserum = TRUE
  )
  
  config_file <- file.path(temp_dir, "valid_config.toml")
  toml_content <- sprintf('
netw_file_path = "%s"
spec_path = "%s"
out_dir = "%s"
nosat = %s
loserum = %s
',
    gsub("\\\\", "/", valid_config$netw_file_path),
    gsub("\\\\", "/", valid_config$spec_path),
    valid_config$out_dir,
    tolower(valid_config$nosat),
    tolower(valid_config$loserum)
  )
  writeLines(toml_content, config_file)
  
  on.exit(unlink(config_file))
  
  result <- validate_autopert_config(config_file)
  
  expect_equal(result$netw_file_path, valid_config$netw_file_path)
  expect_equal(result$spec_path, valid_config$spec_path)
  expect_equal(result$out_dir, valid_config$out_dir)
  expect_equal(result$nosat, FALSE)
  expect_equal(result$loserum, TRUE)
  # Check defaults are applied
  expect_equal(result$missing_nodes_perturbed_overide, FALSE)
  expect_equal(result$missing_nodes_expected_overide, FALSE)
  expect_true(is.na(result$project_path))
  expect_equal(result$group_vars, c("source", "cell_line", "experiment_particular"))
})

test_that("validate_autopert_config applies defaults correctly", {
  # Create minimal config with only required fields
  minimal_config <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "minimal_config.toml")
  toml_content <- sprintf('
netw_file_path = "%s"
spec_path = "%s"
out_dir = "%s"
',
    gsub("\\\\", "/", minimal_config$netw_file_path),
    gsub("\\\\", "/", minimal_config$spec_path),
    minimal_config$out_dir
  )
  writeLines(toml_content, config_file)
  
  on.exit(unlink(config_file))
  
  result <- validate_autopert_config(config_file)
  
  # Check that defaults are applied
  expect_equal(result$nosat, TRUE)
  expect_equal(result$loserum, FALSE)
  expect_equal(result$missing_nodes_perturbed_overide, FALSE)
  expect_equal(result$missing_nodes_expected_overide, FALSE)
  expect_true(is.na(result$project_path))
  expect_equal(result$group_vars, c("source", "cell_line", "experiment_particular"))
})

test_that("validate_autopert_config errors on missing config file", {
  expect_snapshot(
    validate_autopert_config("nonexistent_config.toml"),
    error = TRUE
  )
})

test_that("validate_autopert_config errors on invalid JSON", {
  invalid_toml_file <- file.path(temp_dir, "invalid.toml")
  writeLines("invalid toml [", invalid_toml_file)
  
  on.exit(unlink(invalid_toml_file))
  
  expect_snapshot(
    validate_autopert_config(invalid_toml_file),
    error = TRUE
  )
})

test_that("validate_autopert_config errors on missing required fields", {
  # Test missing netw_file_path
  config_missing_netw <- list(
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_netw.toml")
  toml_content <- sprintf('
spec_path = "%s"
out_dir = "%s"
',
    gsub("\\\\", "/", config_missing_netw$spec_path),
    config_missing_netw$out_dir
  )
  writeLines(toml_content, config_file)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing spec_path
  config_missing_spec <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_spec.toml")
  toml_content <- sprintf('
netw_file_path = "%s"
out_dir = "%s"
',
    gsub("\\\\", "/", config_missing_spec$netw_file_path),
    config_missing_spec$out_dir
  )
  writeLines(toml_content, config_file)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing out_dir
  config_missing_out <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv")
  )
  
  config_file <- file.path(temp_dir, "missing_out.toml")
  toml_content <- sprintf('
netw_file_path = "%s"
spec_path = "%s"
',
    gsub("\\\\", "/", config_missing_out$netw_file_path),
    gsub("\\\\", "/", config_missing_out$spec_path)
  )
  writeLines(toml_content, config_file)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
})

test_that("validate_autopert_config errors on missing referenced files", {
  # Test missing network file
  config_missing_netw_file <- list(
    netw_file_path = "nonexistent_network.json",
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_netw_file.toml")
  toml_content <- sprintf('
netw_file_path = "%s"
spec_path = "%s"
out_dir = "%s"
',
    config_missing_netw_file$netw_file_path,
    gsub("\\\\", "/", config_missing_netw_file$spec_path),
    config_missing_netw_file$out_dir
  )
  writeLines(toml_content, config_file)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing spec file
  config_missing_spec_file <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = "nonexistent_spec.csv",
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_spec_file.toml")
  toml_content <- sprintf('
netw_file_path = "%s"
spec_path = "%s"
out_dir = "%s"
',
    gsub("\\\\", "/", config_missing_spec_file$netw_file_path),
    config_missing_spec_file$spec_path,
    config_missing_spec_file$out_dir
  )
  writeLines(toml_content, config_file)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
})

test_that("validate_autopert_config handles short_filenames parameter correctly", {
  # Test with short_filenames = true
  config_short_filenames <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output",
    short_filenames = TRUE
  )
  
  config_file <- file.path(temp_dir, "short_filenames_true.toml")
  toml_content <- sprintf("
netw_file_path = \"%s\"
spec_path = \"%s\"
out_dir = \"%s\"
short_filenames = %s
",
    gsub("\\\\", "/", config_short_filenames$netw_file_path),
    gsub("\\\\", "/", config_short_filenames$spec_path),
    config_short_filenames$out_dir,
    tolower(as.character(config_short_filenames$short_filenames))
  )
  writeLines(toml_content, config_file)
  
  result <- validate_autopert_config(config_file)
  
  expect_true(result$short_filenames)
  expect_equal(result$netw_file_path, config_short_filenames$netw_file_path)
  expect_equal(result$spec_path, config_short_filenames$spec_path)
  
  unlink(config_file)
  
  # Test with short_filenames = false
  config_short_filenames_false <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output",
    short_filenames = FALSE
  )
  
  config_file <- file.path(temp_dir, "short_filenames_false.toml")
  toml_content <- sprintf("
netw_file_path = \"%s\"
spec_path = \"%s\"
out_dir = \"%s\"
short_filenames = %s
",
    gsub("\\\\", "/", config_short_filenames_false$netw_file_path),
    gsub("\\\\", "/", config_short_filenames_false$spec_path),
    config_short_filenames_false$out_dir,
    tolower(as.character(config_short_filenames_false$short_filenames))
  )
  writeLines(toml_content, config_file)
  
  result <- validate_autopert_config(config_file)
  
  expect_false(result$short_filenames)
  
  unlink(config_file)
  
  # Test without short_filenames (should default to FALSE)
  config_no_short_filenames <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "no_short_filenames.toml")
  toml_content <- sprintf("
netw_file_path = \"%s\"
spec_path = \"%s\"
out_dir = \"%s\"
",
    gsub("\\\\", "/", config_no_short_filenames$netw_file_path),
    gsub("\\\\", "/", config_no_short_filenames$spec_path),
    config_no_short_filenames$out_dir
  )
  writeLines(toml_content, config_file)
  
  result <- validate_autopert_config(config_file)
  
  expect_false(result$short_filenames)  # Should default to FALSE
  
  unlink(config_file)
})
