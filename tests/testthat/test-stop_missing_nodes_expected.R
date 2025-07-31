source(here::here("tests", "testthat", "testing_utils.r"))

test_that("stop_missing_nodes_expected passes when all expected nodes exist in network", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables from example data
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Import spec and verify it has valid expected nodes
  spec <- import_spec(
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    loserum = FALSE,
    clean_underscores = FALSE,
    netw_variables = netw_variables
  )

  # Should not throw error when all expected nodes exist
  expect_no_error(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_expected errors when expected nodes missing from network", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec with missing expected nodes
  spec <- data.frame(
    cell_line = c("test", "test"),
    source = c("test", "test"),
    experiment_particular = c("test", "test"),
    gene = c("missing_expected1", "missing_expected2"),
    perturbation = c(NA, NA),
    expected_result_bma = c(1, 0),
    stringsAsFactors = FALSE
  )

  # Should throw error with missing expected nodes
  expect_snapshot(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})

# test_that("stop_missing_nodes_expected issues warning with override enabled", {
#   setup_log_file()
#   on.exit(cleanup_log_file())
#
#   # Get network variables
#   netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
#
#   # Create spec with missing expected nodes
#   spec <- data.frame(
#     cell_line = c("test", "test"),
#     source = c("test", "test"),
#     experiment_particular = c("test", "test"),
#     gene = c("missing_expected", "a"),
#     perturbation = c(NA, NA),
#     expected_result_bma = c(1, 0),
#     stringsAsFactors = FALSE
#   )
#
#   # Should issue warning but not error with override
#   expect_snapshot(
#     stop_missing_nodes_expected(
#       spec = spec,
#       missing_nodes_expected_overide = TRUE,
#       netw_variables = netw_variables,
#       log_file = log_file
#     )
#   )
# })

test_that("stop_missing_nodes_expected handles mixed valid and invalid nodes", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec with mix of valid and invalid expected nodes
  spec <- data.frame(
    cell_line = rep("test", 4),
    source = rep("test", 4),
    experiment_particular = rep("test", 4),
    gene = c("a", "missing_expected", "b", "another_missing"),
    perturbation = rep(NA, 4),
    expected_result_bma = c(1, 1, 0, 1),
    stringsAsFactors = FALSE
  )

  # Should error mentioning only the missing nodes
  expect_snapshot(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})

test_that("stop_missing_nodes_expected ignores rows without expected results", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec where missing nodes have NA expected results
  spec <- data.frame(
    cell_line = rep("test", 3),
    source = rep("test", 3),
    experiment_particular = rep("test", 3),
    gene = c("a", "missing_node", "b"),
    perturbation = rep(NA, 3),
    expected_result_bma = c(1, NA, 0),  # missing_node has NA expected result
    stringsAsFactors = FALSE
  )

  # Should not error because missing_node has no expected result (NA)
  expect_no_error(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_expected handles empty expected results", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec with no expected results
  spec <- data.frame(
    cell_line = rep("test", 2),
    source = rep("test", 2),
    experiment_particular = rep("test", 2),
    gene = c("a", "b"),
    perturbation = c(1, 0),
    expected_result_bma = c(NA, NA),
    stringsAsFactors = FALSE
  )

  # Should not error when no nodes have expected results
  expect_no_error(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_expected handles duplicate expected genes", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec with duplicate missing expected nodes
  spec <- data.frame(
    cell_line = rep("test", 4),
    source = rep("test", 4),
    experiment_particular = rep("test", 4),
    gene = c("missing_expected", "a", "missing_expected", "b"),
    perturbation = rep(NA, 4),
    expected_result_bma = c(1, 1, 0, 1),
    stringsAsFactors = FALSE
  )

  # Should report missing_expected only once
  expect_snapshot(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})

test_that("stop_missing_nodes_expected handles nodes that are both perturbed and expected", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec with missing nodes that have both perturbation and expected result
  spec <- data.frame(
    cell_line = rep("test", 2),
    source = rep("test", 2),
    experiment_particular = rep("test", 2),
    gene = c("missing_combo", "a"),
    perturbation = c(1, 1),
    expected_result_bma = c(1, 0),
    stringsAsFactors = FALSE
  )

  # Should error because missing_combo has expected result
  expect_snapshot(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})

test_that("stop_missing_nodes_expected message format is correct", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))

  # Create spec with specific missing expected nodes to test message format
  spec <- data.frame(
    cell_line = rep("test", 3),
    source = rep("test", 3),
    experiment_particular = rep("test", 3),
    gene = c("missing_A", "missing_B", "a"),
    perturbation = rep(NA, 3),
    expected_result_bma = c(1, 0, 1),
    stringsAsFactors = FALSE
  )

  # Test that error message contains expected format elements
  expect_snapshot(
    stop_missing_nodes_expected(
      spec = spec,
      missing_nodes_expected_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})