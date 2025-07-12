source(here::here("tests", "testthat", "testing_utils.r"))

test_that("stop_missing_nodes_perturbed passes when all perturbed nodes exist in network", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables from example data
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Import spec and verify it has valid perturbed nodes
  spec <- import_spec(
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    loserum = FALSE,
    clean_underscores = FALSE,
    netw_variables = netw_variables
  )
  
  # Should not throw error when all perturbed nodes exist
  expect_no_error(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_perturbed errors when perturbed nodes missing from network", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Create spec with missing nodes
  spec <- data.frame(
    cell_line = c("test", "test"),
    source = c("test", "test"),
    experiment_particular = c("test", "test"),
    gene = c("missing_node1", "missing_node2"),
    perturbation = c(1, 0),
    expected_result_bma = c(NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Should throw error with missing nodes
  expect_snapshot(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})

test_that("stop_missing_nodes_perturbed issues warning with override enabled", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Create spec with missing nodes
  spec <- data.frame(
    cell_line = c("test", "test"),
    source = c("test", "test"),
    experiment_particular = c("test", "test"),
    gene = c("missing_node1", "existing_node"),
    perturbation = c(1, 0),
    expected_result_bma = c(NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Should issue warning but not error with override
  expect_snapshot(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = TRUE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_perturbed handles mixed valid and invalid nodes", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Create spec with mix of valid and invalid nodes
  spec <- data.frame(
    cell_line = rep("test", 4),
    source = rep("test", 4),
    experiment_particular = rep("test", 4),
    gene = c("a", "missing_node", "b", "another_missing"),
    perturbation = c(1, 1, 0, 1),
    expected_result_bma = rep(NA, 4),
    stringsAsFactors = FALSE
  )
  
  # Should error mentioning only the missing nodes
  expect_snapshot(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})

test_that("stop_missing_nodes_perturbed ignores rows without perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Create spec where missing nodes have NA perturbations
  spec <- data.frame(
    cell_line = rep("test", 3),
    source = rep("test", 3),
    experiment_particular = rep("test", 3),
    gene = c("a", "missing_node", "b"),
    perturbation = c(1, NA, 0),  # missing_node has NA perturbation
    expected_result_bma = rep(NA, 3),
    stringsAsFactors = FALSE
  )
  
  # Should not error because missing_node is not perturbed (NA perturbation)
  expect_no_error(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_perturbed handles empty perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Create spec with no perturbations
  spec <- data.frame(
    cell_line = rep("test", 2),
    source = rep("test", 2),
    experiment_particular = rep("test", 2),
    gene = c("a", "b"),
    perturbation = c(NA, NA),
    expected_result_bma = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  # Should not error when no nodes are perturbed
  expect_no_error(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    )
  )
})

test_that("stop_missing_nodes_perturbed handles duplicate perturbed genes", {
  setup_log_file()
  on.exit(cleanup_log_file())
  
  # Get network variables
  netw_variables <- get_netw_variables(here::here("examples", "autopert", "helper_autopert_1.json"))
  
  # Create spec with duplicate missing nodes
  spec <- data.frame(
    cell_line = rep("test", 4),
    source = rep("test", 4),
    experiment_particular = rep("test", 4),
    gene = c("missing_node", "a", "missing_node", "b"),
    perturbation = c(1, 1, 0, 1),
    expected_result_bma = rep(NA, 4),
    stringsAsFactors = FALSE
  )
  
  # Should report missing_node only once
  expect_snapshot(
    stop_missing_nodes_perturbed(
      spec = spec,
      missing_nodes_perturbed_overide = FALSE,
      netw_variables = netw_variables,
      log_file = log_file
    ),
    error = TRUE
  )
})