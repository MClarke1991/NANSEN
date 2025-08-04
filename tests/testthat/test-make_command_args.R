test_that("make_command_args works with default column names", {
  # Create test data frame
  test_df <- data.frame(
    id = c(1, 2, 3),
    activity = c(0, 1, 2),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("command_arg", "filename_part") %in% names(result)))
  expect_equal(nrow(result), 3)

  # Check command_arg format: "-ko id activity"
  expect_equal(result$command_arg[1], "-ko 1 0")
  expect_equal(result$command_arg[2], "-ko 2 1")
  expect_equal(result$command_arg[3], "-ko 3 2")

  # Check filename_part format: "name__activity"
  expect_equal(result$filename_part[1], "node1__0")
  expect_equal(result$filename_part[2], "node2__1")
  expect_equal(result$filename_part[3], "node3__2")
})

test_that("make_command_args works with custom column names", {
  # Create test data frame with custom column names
  test_df <- data.frame(
    node_id = c(10, 20),
    level = c(1, 0),
    gene_name = c("geneA", "geneB"),
    stringsAsFactors = FALSE
  )

  result <- make_command_args(
    test_df,
    id_col = "node_id",
    activity_col = "level",
    node_col = "gene_name"
  )

  expect_equal(result$command_arg[1], "-ko 10 1")
  expect_equal(result$command_arg[2], "-ko 20 0")
  expect_equal(result$filename_part[1], "geneA__1")
  expect_equal(result$filename_part[2], "geneB__0")
})

test_that("make_command_args preserves other columns", {
  # Create test data frame with additional columns
  test_df <- data.frame(
    id = 1,
    activity = 2,
    name = "test_node",
    extra_col = "extra_value",
    another_col = 42,
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df)

  # Should preserve all original columns plus add new ones
  expect_true("extra_col" %in% names(result))
  expect_true("another_col" %in% names(result))
  expect_equal(result$extra_col[1], "extra_value")
  expect_equal(result$another_col[1], 42)
})

test_that("make_command_args handles missing columns", {
  # Create test data frame missing required columns
  test_df <- data.frame(
    not_id = 1,
    not_activity = 2,
    not_name = "test",
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    make_command_args(test_df),
    error = TRUE
  )
})

test_that("make_command_args handles empty data frame", {
  # Create empty data frame with correct structure
  test_df <- data.frame(
    id = integer(0),
    activity = integer(0),
    name = character(0),
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("command_arg", "filename_part") %in% names(result)))
})

test_that("make_command_args handles special characters in names", {
  # Create test data frame with special characters
  test_df <- data.frame(
    id = c(1, 2, 3),
    activity = c(0, 1, 2),
    name = c("node-1", "node_2", "node.3"),
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df)

  # Should handle special characters in filename parts
  expect_equal(result$filename_part[1], "node-1__0")
  expect_equal(result$filename_part[2], "node_2__1")
  expect_equal(result$filename_part[3], "node.3__2")

  # Command args should work with any node names
  expect_equal(result$command_arg[1], "-ko 1 0")
  expect_equal(result$command_arg[2], "-ko 2 1")
  expect_equal(result$command_arg[3], "-ko 3 2")
})

test_that("make_command_args handles mixed data types", {
  # Create test data frame with mixed types
  test_df <- data.frame(
    id = c(1L, 2L),  # integer
    activity = c(0.0, 2.0),  # numeric
    name = c("node1", "node2"),  # character
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df)

  # Should handle numeric activity values
  expect_equal(result$command_arg[1], "-ko 1 0")
  expect_equal(result$command_arg[2], "-ko 2 2")
  expect_equal(result$filename_part[1], "node1__0")
  expect_equal(result$filename_part[2], "node2__2")
})

test_that("make_command_args throws error for non-numeric and NA values", {
  # Test non-numeric id column
  test_df_char_id <- data.frame(
    id = c("a", "b", "c"),
    activity = c(0, 1, 2),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_error(make_command_args(test_df_char_id),
               "Column id must be numeric. Found values: a, b, c")

  # Test non-numeric activity column
  test_df_char_activity <- data.frame(
    id = c(1, 2, 3),
    activity = c("x", "y", "z"),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_error(make_command_args(test_df_char_activity),
               "Column activity must be numeric. Found values: x, y, z")

  # Test NA values in id column
  test_df_na_id <- data.frame(
    id = c(1, 2, NA),
    activity = c(0, 1, 2),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_error(make_command_args(test_df_na_id),
               "Column id contains NA values at positions: 3")

  # Test NA values in activity column
  test_df_na_activity <- data.frame(
    id = c(1, 2, 3),
    activity = c(0, NA, 2),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_error(make_command_args(test_df_na_activity),
               "Column activity contains NA values at positions: 2")

  # Test multiple NA values
  test_df_multiple_na <- data.frame(
    id = c(1, NA, NA),
    activity = c(0, 1, 2),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_error(make_command_args(test_df_multiple_na),
               "Column id contains NA values at positions: 2, 3")

  # Test that valid numeric data works
  test_df_valid <- data.frame(
    id = c(1, 2, 3),
    activity = c(0, 1, 2),
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df_valid)
  expect_equal(nrow(result), 3)
  expect_true(all(c("command_arg", "filename_part") %in% names(result)))
})

test_that("make_command_args throws error for non-integer activity values", {
  source("testing_utils.r")
  setup_log_file()
  on.exit(cleanup_log_file())

  # Test non-integer activity values
  test_df_non_integer <- data.frame(
    id = c(1, 2, 3),
    activity = c(0.5, 1.2, 2.7),  # non-integer values
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    make_command_args(test_df_non_integer),
    error = TRUE
  )
})

test_that("make_command_args works with integer activity values including edge cases", {
  source("testing_utils.r")
  setup_log_file()
  on.exit(cleanup_log_file())

  # Test valid integer values including zero and negative
  test_df_valid_integers <- data.frame(
    id = c(1, 2, 3, 4),
    activity = c(0, 1, -1, 100),  # valid integer values
    name = c("node1", "node2", "node3", "node4"),
    stringsAsFactors = FALSE
  )

  result <- make_command_args(test_df_valid_integers)
  expect_equal(nrow(result), 4)
  expect_true(all(c("command_arg", "filename_part") %in% names(result)))

  # Check that values are preserved
  expect_equal(result$command_arg[1], "-ko 1 0")
  expect_equal(result$command_arg[2], "-ko 2 1")
  expect_equal(result$command_arg[3], "-ko 3 -1")
  expect_equal(result$command_arg[4], "-ko 4 100")
})

test_that("make_command_args handles mix of valid and invalid activity values", {
  source("testing_utils.r")
  setup_log_file()
  on.exit(cleanup_log_file())

  # Test mix of integer and non-integer values
  test_df_mixed <- data.frame(
    id = c(1, 2, 3),
    activity = c(1, 2.5, 3),  # one non-integer value
    name = c("node1", "node2", "node3"),
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    make_command_args(test_df_mixed),
    error = TRUE
  )
})
