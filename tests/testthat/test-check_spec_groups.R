source(here::here("tests", "testthat", "testing_utils.r"))

test_that("check_spec_groups accepts properly grouped experiments", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Valid consecutive groups
  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellA", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment1", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneZ", "geneX", "geneY")
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups accepts single experiment", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Single experiment with multiple rows
  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1"),
    cell_line = c("cellA", "cellA", "cellA"),
    experiment_particular = c("treatment1", "treatment1", "treatment1"),
    gene = c("geneX", "geneY", "geneZ")
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups accepts single row experiments", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Multiple experiments with one row each
  spec <- tibble::tibble(
    source = c("exp1", "exp2", "exp3"),
    cell_line = c("cellA", "cellB", "cellC"),
    experiment_particular = c("treatment1", "treatment2", "treatment3"),
    gene = c("geneX", "geneY", "geneZ")
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups accepts empty dataframe", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Empty specification
  spec <- tibble::tibble(
    source = character(0),
    cell_line = character(0),
    experiment_particular = character(0),
    gene = character(0)
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups accepts custom group variables", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Using different grouping variables
  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp2", "exp2"),
    cell_line = c("cellA", "cellA", "cellB", "cellB"),
    experiment_particular = c("treatment1", "treatment1", "treatment2", "treatment2"),
    gene = c("geneX", "geneY", "geneX", "geneY"),
    batch = c("batch1", "batch1", "batch2", "batch2")
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "batch"))))
})

test_that("check_spec_groups accepts mixed experiment sizes", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Some experiments with 1 row, others with multiple rows
  spec <- tibble::tibble(
    source = c("exp1", "exp2", "exp2", "exp2", "exp3"),
    cell_line = c("cellA", "cellB", "cellB", "cellB", "cellC"),
    experiment_particular = c("treatment1", "treatment2", "treatment2", "treatment2", "treatment3"),
    gene = c("geneX", "geneY", "geneZ", "geneW", "geneX")
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups accepts single row dataframe", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Single row
  spec <- tibble::tibble(
    source = "exp1",
    cell_line = "cellA",
    experiment_particular = "treatment1",
    gene = "geneX"
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups accepts all same groups", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # All rows have identical grouping
  spec <- tibble::tibble(
    source = c("exp1", "exp1", "exp1"),
    cell_line = c("cellA", "cellA", "cellA"),
    experiment_particular = c("treatment1", "treatment1", "treatment1"),
    gene = c("geneX", "geneY", "geneZ")
  )

  expect_silent(suppressMessages(check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))))
})

test_that("check_spec_groups errors on non-consecutive duplicates", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Same experiment scattered across non-consecutive blocks
  spec <- tibble::tibble(
    source = c("exp1", "exp2", "exp1", "exp2"),
    cell_line = c("cellA", "cellB", "cellA", "cellB"),
    experiment_particular = c("treatment1", "treatment2", "treatment1", "treatment2"),
    gene = c("geneX", "geneX", "geneY", "geneY")
  )

  expect_snapshot(check_spec_groups(spec, c("source", "cell_line", "experiment_particular")), error = TRUE)
})

test_that("check_spec_groups errors on multiple scattered experiments", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Multiple experiments with non-consecutive blocks
  spec <- tibble::tibble(
    source = c("exp1", "exp2", "exp3", "exp1", "exp2"),
    cell_line = c("cellA", "cellB", "cellC", "cellA", "cellB"),
    experiment_particular = c("treatment1", "treatment2", "treatment3", "treatment1", "treatment2"),
    gene = c("geneX", "geneY", "geneZ", "geneY", "geneZ")
  )

  expect_snapshot(check_spec_groups(spec, c("source", "cell_line", "experiment_particular")), error = TRUE)
})

test_that("check_spec_groups errors on interleaved experiments", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Experiments with rows interleaved with other experiments
  spec <- tibble::tibble(
    source = c("exp1", "exp2", "exp1", "exp2", "exp1"),
    cell_line = c("cellA", "cellB", "cellA", "cellB", "cellA"),
    experiment_particular = c("treatment1", "treatment2", "treatment1", "treatment2", "treatment1"),
    gene = c("geneX", "geneY", "geneY", "geneZ", "geneZ")
  )

  expect_snapshot(check_spec_groups(spec, c("source", "cell_line", "experiment_particular")), error = TRUE)
})