source("testing_utils.r")

test_that("make_bkg_commands_combo creates correct background commands", {

  # Create test backgrounds data
  backgrounds <- tibble::tibble(
    background = c("wt", "wt", "cancer", "cancer"),
    name = c("growth_factor", "a", "growth_factor", "e"),
    activity = c(2, 1, 2, 0)
  )

  # Create test network variables
  netw_variables <- tibble::tibble(
    name = c("growth_factor", "a", "b", "c", "d", "e"),
    id = c(3, 4, 5, 9, 13, 19),
    range_from = c(0, 0, 0, 0, 0, 0),
    range_to = c(2, 2, 4, 10, 2, 1)
  )

  result <- make_bkg_commands_combo(backgrounds, netw_variables)

  # Check structure
  expect_true(all(c("background", "filename_prefix", "command_arg") %in% colnames(result)))
  expect_equal(nrow(result), 2) # Two unique backgrounds

  # Check background names
  expect_true("wt" %in% result$background)
  expect_true("cancer" %in% result$background)

  # Check command arguments are properly formatted
  wt_row <- result[result$background == "wt", ]
  cancer_row <- result[result$background == "cancer", ]

  # For wt background: growth_factor and a
  expect_true(grepl("-ko 3 2", wt_row$command_arg))
  expect_true(grepl("-ko 4 1", wt_row$command_arg))

  # For cancer background: growth_factor and e
  expect_true(grepl("-ko 3 2", cancer_row$command_arg))
  expect_true(grepl("-ko 19 0", cancer_row$command_arg))

  # Check filename prefixes contain node names and activities
  expect_true(grepl("growth_factor__2", wt_row$filename_prefix))
  expect_true(grepl("a__1", wt_row$filename_prefix))
  expect_true(grepl("growth_factor__2", cancer_row$filename_prefix))
  expect_true(grepl("e__0", cancer_row$filename_prefix))
})

test_that("make_bkg_commands_combo works with custom node column", {

  backgrounds <- tibble::tibble(
    background = c("test_bg"),
    gene = c("a"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    gene = c("a", "b"),
    id = c(4, 5),
    range_from = c(0, 0),
    range_to = c(2, 4)
  )

  result <- make_bkg_commands_combo(backgrounds, netw_variables, node_col = "gene")

  expect_equal(nrow(result), 1)
  expect_equal(result$background, "test_bg")
  expect_true(grepl("-ko 4 1", result$command_arg))
  expect_true(grepl("a__1", result$filename_prefix))
})

test_that("make_bkg_commands_combo handles empty backgrounds", {

  backgrounds <- tibble::tibble(
    background = character(),
    name = character(),
    activity = numeric()
  )

  netw_variables <- tibble::tibble(
    name = c("a", "b"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2)
  )

  result <- make_bkg_commands_combo(backgrounds, netw_variables)

  expect_equal(nrow(result), 0)
  expect_true(all(c("background", "filename_prefix", "command_arg") %in% colnames(result)))
})

test_that("make_bkg_commands_combo combines multiple nodes per background correctly", {

  # Test background with three nodes
  backgrounds <- tibble::tibble(
    background = c("multi", "multi", "multi"),
    name = c("a", "b", "c"),
    activity = c(0, 1, 2)
  )

  netw_variables <- tibble::tibble(
    name = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2)
  )

  result <- make_bkg_commands_combo(backgrounds, netw_variables)

  expect_equal(nrow(result), 1)
  expect_equal(result$background, "multi")

  # Check all three commands are combined
  expect_true(grepl("-ko 1 0", result$command_arg))
  expect_true(grepl("-ko 2 1", result$command_arg))
  expect_true(grepl("-ko 3 2", result$command_arg))

  # Check filename includes all parts
  expect_true(grepl("a__0", result$filename_prefix))
  expect_true(grepl("b__1", result$filename_prefix))
  expect_true(grepl("c__2", result$filename_prefix))
})

test_that("make_bkg_commands_combo errors when node_col missing from backgrounds", {

  backgrounds <- tibble::tibble(
    background = c("test"),
    wrong_name = c("a"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    name = c("a"),
    id = c(1),
    range_from = c(0),
    range_to = c(2)
  )

  expect_snapshot(
    make_bkg_commands_combo(backgrounds, netw_variables),
    error = TRUE
  )
})

test_that("make_bkg_commands_combo errors when node_col missing from netw_variables", {

  backgrounds <- tibble::tibble(
    background = c("test"),
    name = c("a"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    wrong_name = c("a"),
    id = c(1),
    range_from = c(0),
    range_to = c(2)
  )

  expect_snapshot(
    make_bkg_commands_combo(backgrounds, netw_variables),
    error = TRUE
  )
})

test_that("make_bkg_commands_combo throws error for nodes not in network", {

  backgrounds <- tibble::tibble(
    background = c("test"),
    name = c("missing_node"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    name = c("a", "b"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2)
  )

  # Should throw an error because missing_node will have NA for id after left_join
  expect_error(make_bkg_commands_combo(backgrounds, netw_variables),
               "Column id contains NA values at positions: 1")
})

test_that("make_bkg_commands_combo throws error for non-integer activity values", {

  backgrounds <- tibble::tibble(
    background = c("test", "test", "test"),
    name = c("a", "b", "c"),
    activity = c(0.5, 1.2, 2.7)  # non-integer values
  )

  netw_variables <- tibble::tibble(
    name = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2)
  )

  expect_snapshot(
    make_bkg_commands_combo(backgrounds, netw_variables),
    error = TRUE
  )
})

test_that("make_bkg_commands_combo works with integer activity values", {

  backgrounds <- tibble::tibble(
    background = c("test", "test", "test"),
    name = c("a", "b", "c"),
    activity = c(0, 1, 2)  # valid integer values
  )

  netw_variables <- tibble::tibble(
    name = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2)
  )

  result <- make_bkg_commands_combo(backgrounds, netw_variables)
  
  expect_equal(nrow(result), 1)
  expect_true(all(c("background", "filename_prefix", "command_arg") %in% colnames(result)))
  expect_equal(result$background, "test")
})

test_that("make_bkg_commands_combo handles mix of valid and invalid activity values", {

  backgrounds <- tibble::tibble(
    background = c("test", "test", "test"),
    name = c("a", "b", "c"),
    activity = c(1, 2.5, 3)  # one non-integer value
  )

  netw_variables <- tibble::tibble(
    name = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2)
  )

  expect_snapshot(
    make_bkg_commands_combo(backgrounds, netw_variables),
    error = TRUE
  )
})