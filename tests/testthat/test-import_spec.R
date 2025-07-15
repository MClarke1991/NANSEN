source(here::here("tests", "testthat", "testing_utils.r"))

test_that("import_spec imports specification correctly with basic inputs", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables from example data
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Import spec with default settings
  result <- import_spec(
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    loserum = FALSE,
    clean_underscores = FALSE,
    netw_variables = netw_variables
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expected_cols <- c("cell_line", "paper_title", "Paper DOI", "source", 
                    "experiment_overview", "experiment_particular", "gene", 
                    "perturbation", "expected_result_bma",
                    "id", "range_from", "range_to", "formula")
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check data types
  expect_true(is.factor(result$experiment_particular))
  expect_true(is.integer(result$range_from))
  expect_true(is.integer(result$range_to))
  
  # Check that network variables were joined correctly
  expect_true(all(!is.na(result$id[result$gene %in% netw_variables$name])))
})

test_that("import_spec handles loserum option correctly", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Create a mock spec with SERUM gene
  temp_spec <- tempfile(fileext = ".csv")
  spec_data <- data.frame(
    cell_line = "test",
    paper_title = "test",
    `Paper DOI` = "test",
    source = "test",
    experiment_overview = "test",
    experiment_particular = "test",
    gene = c("SERUM", "a"),
    perturbation = c(0, 1),
    expected_result_bma = c(NA, NA),
    notes = c("", ""),
    check.names = FALSE
  )
  readr::write_csv(spec_data, temp_spec)
  on.exit(file.remove(temp_spec), add = TRUE)
  
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Test with loserum = TRUE
  expect_snapshot(
    result_loserum <- import_spec(
      spec_path = temp_spec,
      loserum = TRUE,
      clean_underscores = FALSE,
      netw_variables = netw_variables
    )
  )
  
  # Check that SERUM perturbation was set to 1
  serum_row <- result_loserum[result_loserum$gene == "SERUM", ]
  expect_equal(serum_row$perturbation, 1)
})

test_that("import_spec handles clean_underscores option correctly", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Create a mock spec with underscore prefixes
  temp_spec <- tempfile(fileext = ".csv")
  spec_data <- data.frame(
    cell_line = c("_test_cell", "normal_cell"),
    paper_title = "test",
    `Paper DOI` = "test",
    source = c("_test_source", "normal_source"),
    experiment_overview = "test",
    experiment_particular = "test",
    gene = c("a", "b"),
    perturbation = c(1, 0),
    expected_result_bma = c(NA, NA),
    notes = c("", ""),
    check.names = FALSE
  )
  readr::write_csv(spec_data, temp_spec)
  on.exit(file.remove(temp_spec), add = TRUE)
  
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Test with clean_underscores = TRUE
  result <- import_spec(
    spec_path = temp_spec,
    loserum = FALSE,
    clean_underscores = TRUE,
    netw_variables = netw_variables
  )
  
  # Check that underscores were removed
  expect_equal(result$cell_line[1], "test_cell")
  expect_equal(result$source[1], "test_source")
  expect_equal(result$cell_line[2], "normal_cell")
  expect_equal(result$source[2], "normal_source")
})

test_that("import_spec preserves experiment_particular factor order", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Create a spec with specific order
  temp_spec <- tempfile(fileext = ".csv")
  spec_data <- data.frame(
    cell_line = rep("test", 4),
    paper_title = rep("test", 4),
    `Paper DOI` = rep("test", 4),
    source = rep("test", 4),
    experiment_overview = rep("test", 4),
    experiment_particular = c("exp_c", "exp_a", "exp_b", "exp_a"),
    gene = c("a", "b", "c", "a"),
    perturbation = c(1, 1, 1, 0),
    expected_result_bma = rep(NA, 4),
    notes = rep("", 4),
    check.names = FALSE
  )
  readr::write_csv(spec_data, temp_spec)
  on.exit(file.remove(temp_spec), add = TRUE)
  
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  result <- import_spec(
    spec_path = temp_spec,
    loserum = FALSE,
    clean_underscores = FALSE,
    netw_variables = netw_variables
  )
  
  # Check that factor levels preserve original order
  expected_levels <- c("exp_c", "exp_a", "exp_b")
  expect_equal(levels(result$experiment_particular), expected_levels)
})

test_that("import_spec handles missing file gracefully", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  expect_error(
    import_spec(
      spec_path = "nonexistent_file.csv",
      loserum = FALSE,
      clean_underscores = FALSE,
      netw_variables = netw_variables
    )
  )
})

test_that("import_spec handles empty specification file", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Create empty CSV file
  temp_spec <- tempfile(fileext = ".csv")
  writeLines("", temp_spec)
  on.exit(file.remove(temp_spec), add = TRUE)
  
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  expect_error(
    import_spec(
      spec_path = temp_spec,
      loserum = FALSE,
      clean_underscores = FALSE,
      netw_variables = netw_variables
    )
  )
})

test_that("import_spec joins network variables correctly", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  result <- import_spec(
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    loserum = FALSE,
    clean_underscores = FALSE,
    netw_variables = netw_variables
  )
  
  # Check that genes in network have corresponding network variable data
  network_genes <- result[result$gene %in% netw_variables$name, ]
  expect_true(all(!is.na(network_genes$id)))
  expect_true(all(!is.na(network_genes$range_from)))
  expect_true(all(!is.na(network_genes$range_to)))
  
  # Check that genes not in network have NA values for network variables
  non_network_genes <- result[!result$gene %in% netw_variables$name, ]
  if (nrow(non_network_genes) > 0) {
    expect_true(all(is.na(non_network_genes$id)))
  }
})