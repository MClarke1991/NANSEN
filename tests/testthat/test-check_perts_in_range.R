source(here::here("tests", "testthat", "testing_utils.r"))

test_that("check_perts_in_range accepts valid perturbations and expectations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Valid data within range
  spec_levels <- tibble::tibble(
    source = c("test1", "test2", "test3"),
    experiment_particular = c("exp1", "exp2", "exp3"),
    gene = c("gene1", "gene2", "gene3"),
    perturbation_bma = c(0, 1, 2),
    expectation_bma = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(3, 3, 3)
  )

  expect_silent(check_perts_in_range(spec_levels))
})

test_that("check_perts_in_range accepts boundary values", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Test exact boundary values
  spec_levels <- tibble::tibble(
    source = c("test1", "test2"),
    experiment_particular = c("exp1", "exp2"),
    gene = c("gene1", "gene2"),
    perturbation_bma = c(0, 2),  # exactly at boundaries
    expectation_bma = c(2, 0),   # exactly at boundaries
    range_from = c(0, 0),
    range_to = c(2, 2)
  )

  expect_silent(check_perts_in_range(spec_levels))
})

test_that("check_perts_in_range handles NA values correctly", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # NA values should be allowed (they don't trigger range checks)
  spec_levels <- tibble::tibble(
    source = c("test1", "test2", "test3"),
    experiment_particular = c("exp1", "exp2", "exp3"),
    gene = c("gene1", "gene2", "gene3"),
    perturbation_bma = c(NA, 1, 2),
    expectation_bma = c(1, NA, 3),
    range_from = c(0, 0, 0),
    range_to = c(3, 3, 3)
  )

  expect_silent(check_perts_in_range(spec_levels))
})

test_that("check_perts_in_range errors when perturbations are below range", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = "test1",
    experiment_particular = "exp1",
    gene = "gene1",
    perturbation_bma = -1,  # below range
    expectation_bma = 1,
    range_from = 0,
    range_to = 3
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range errors when perturbations are above range", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = "test1",
    experiment_particular = "exp1",
    gene = "gene1",
    perturbation_bma = 4,  # above range
    expectation_bma = 1,
    range_from = 0,
    range_to = 3
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range errors when expectations are below range", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = "test1",
    experiment_particular = "exp1",
    gene = "gene1",
    perturbation_bma = 1,
    expectation_bma = -1,  # below range
    range_from = 0,
    range_to = 3
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range errors when expectations are above range", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = "test1",
    experiment_particular = "exp1",
    gene = "gene1",
    perturbation_bma = 1,
    expectation_bma = 4,  # above range
    range_from = 0,
    range_to = 3
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range errors with multiple out-of-range perturbations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = c("test1", "test2"),
    experiment_particular = c("exp1", "exp2"),
    gene = c("gene1", "gene2"),
    perturbation_bma = c(-1, 5),  # both out of range
    expectation_bma = c(1, 2),
    range_from = c(0, 0),
    range_to = c(3, 3)
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range errors with multiple out-of-range expectations", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = c("test1", "test2"),
    experiment_particular = c("exp1", "exp2"),
    gene = c("gene1", "gene2"),
    perturbation_bma = c(1, 2),
    expectation_bma = c(-1, 5),  # both out of range
    range_from = c(0, 0),
    range_to = c(3, 3)
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range handles mixed valid and invalid data", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = c("test1", "test2", "test3"),
    experiment_particular = c("exp1", "exp2", "exp3"),
    gene = c("gene1", "gene2", "gene3"),
    perturbation_bma = c(1, -1, 2),  # middle one out of range
    expectation_bma = c(2, 1, 3),
    range_from = c(0, 0, 0),
    range_to = c(3, 3, 3)
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range handles different ranges per gene", {
  setup_log_file()
  on.exit(cleanup_log_file())

  # Test that each gene can have different valid ranges
  spec_levels <- tibble::tibble(
    source = c("test1", "test2", "test3"),
    experiment_particular = c("exp1", "exp2", "exp3"),
    gene = c("gene1", "gene2", "gene3"),
    perturbation_bma = c(1, 5, 10),
    expectation_bma = c(2, 6, 11),
    range_from = c(0, 5, 10),     # different ranges
    range_to = c(3, 8, 15)        # different ranges
  )

  expect_silent(check_perts_in_range(spec_levels))
})

test_that("check_perts_in_range errors when gene has perturbation outside its specific range", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = c("test1", "test2"),
    experiment_particular = c("exp1", "exp2"),
    gene = c("gene1", "gene2"),
    perturbation_bma = c(1, 3),   # gene2 pert outside its range
    expectation_bma = c(2, 1),
    range_from = c(0, 0),
    range_to = c(3, 2)            # gene2 has max of 2
  )

  expect_snapshot(check_perts_in_range(spec_levels), error = TRUE)
})

test_that("check_perts_in_range works with single row data", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = "test1",
    experiment_particular = "exp1",
    gene = "gene1",
    perturbation_bma = 1,
    expectation_bma = 2,
    range_from = 0,
    range_to = 3
  )

  expect_silent(check_perts_in_range(spec_levels))
})

test_that("check_perts_in_range handles empty data frame", {
  setup_log_file()
  on.exit(cleanup_log_file())

  spec_levels <- tibble::tibble(
    source = character(0),
    experiment_particular = character(0),
    gene = character(0),
    perturbation_bma = numeric(0),
    expectation_bma = numeric(0),
    range_from = numeric(0),
    range_to = numeric(0)
  )

  expect_silent(check_perts_in_range(spec_levels))
})