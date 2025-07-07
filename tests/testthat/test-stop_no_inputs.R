source("testing_utils.r")

test_that("stop_no_inputs accepts spec with all experiments having perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellA", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment1", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneZ", "geneX", "geneY"),
    perturbation = c("min", "max", "mid", "5", "7")
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_silent(stop_no_inputs(spec, log_file, group_vars))
})

test_that("stop_no_inputs accepts spec with mixed NA and non-NA perturbations per experiment", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1", "exp2", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellA", "cellB", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment1", "treatment2", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneZ", "geneX", "geneY", "geneZ"),
    perturbation = c("min", NA, "max", NA, "mid", NA)
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_silent(stop_no_inputs(spec, log_file, group_vars))
})

test_that("stop_no_inputs accepts single experiment with perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1"),
    cell_line = c("cellA", "cellA", "cellA"),
    experiment_particular = c("treatment1", "treatment1", "treatment1"),
    gene = c("geneX", "geneY", "geneZ"),
    perturbation = c("min", "max", "mid")
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_silent(stop_no_inputs(spec, log_file, group_vars))
})

test_that("stop_no_inputs accepts empty dataframe", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = character(0),
    cell_line = character(0),
    experiment_particular = character(0),
    gene = character(0),
    perturbation = character(0)
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_silent(stop_no_inputs(spec, log_file, group_vars))
})

test_that("stop_no_inputs accepts experiments with different group_vars", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneX", "geneY"),
    perturbation = c("min", NA, "max", "mid"),
    batch = c("batch1", "batch1", "batch2", "batch2")
  )
  
  # Using different grouping variables
  group_vars <- c("source", "batch")
  
  expect_silent(stop_no_inputs(spec, log_file, group_vars))
})

test_that("stop_no_inputs errors when experiment has all NA perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellA", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment1", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneZ", "geneX", "geneY"),
    perturbation = c(NA, NA, NA, "min", "max")
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_snapshot(stop_no_inputs(spec, log_file, group_vars), error = TRUE)
})

test_that("stop_no_inputs errors when multiple experiments have all NA perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp2", "exp2", "exp3", "exp3"),
    cell_line = c("cellA", "cellA", "cellB", "cellB", "cellC", "cellC"),
    experiment_particular = c("treatment1", "treatment1", "treatment2", "treatment2", "treatment3", "treatment3"),
    gene = c("geneX", "geneY", "geneX", "geneY", "geneX", "geneY"),
    perturbation = c(NA, NA, "min", "max", NA, NA)
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_snapshot(stop_no_inputs(spec, log_file, group_vars), error = TRUE)
})

test_that("stop_no_inputs errors when single row experiment has NA perturbation", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = "exp1",
    cell_line = "cellA",
    experiment_particular = "treatment1",
    gene = "geneX",
    perturbation = NA
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_snapshot(stop_no_inputs(spec, log_file, group_vars), error = TRUE)
})

test_that("stop_no_inputs errors with different group_vars when experiment has all NA", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneX", "geneY"),
    perturbation = c(NA, NA, "min", "max"),
    batch = c("batch1", "batch1", "batch2", "batch2")
  )
  
  # Using different grouping - exp1 has all NA perturbations
  group_vars <- c("source", "batch")
  
  expect_snapshot(stop_no_inputs(spec, log_file, group_vars), error = TRUE)
})

test_that("stop_no_inputs handles numeric perturbations correctly", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1"),
    cell_line = c("cellA", "cellA", "cellA"),
    experiment_particular = c("treatment1", "treatment1", "treatment1"),
    gene = c("geneX", "geneY", "geneZ"),
    perturbation = c(5, 7.5, 10)
  )
  
  group_vars <- c("source", "cell_line", "experiment_particular")
  
  expect_silent(stop_no_inputs(spec, log_file, group_vars))
})