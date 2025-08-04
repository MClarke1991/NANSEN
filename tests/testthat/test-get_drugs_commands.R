source("testing_utils.r")

test_that("get_drugs_commands generates correct basic output", {

  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("a", "b"),
    activity = c(0, 1)
  )

  netw_variables <- tibble::tibble(
    node = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2),
    formula = c("formula_a", "formula_b", "formula_c")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # Check structure
  expected_cols <- c("drug", "node", "activity", "drug_name_original", "id",
                     "range_from", "range_to", "formula", "command_arg",
                     "alt_filename_part", "filename_part")
  expect_true(all(expected_cols %in% colnames(result)))

  # Check command_arg format
  expect_equal(result$command_arg[1], "-ko 1 0")
  expect_equal(result$command_arg[2], "-ko 2 1")

  # Check filename parts
  expect_equal(result$filename_part[1], "drug_a__NA")
  expect_equal(result$filename_part[2], "drug_b__NA")
})

test_that("get_drugs_commands sanitizes drug names", {

  drugs <- tibble::tibble(
    drug = c("Drug A!", "Drug-B/C", "Drug.D E"),
    node = c("a", "b", "c"),
    activity = c(0, 1, 2)
  )

  netw_variables <- tibble::tibble(
    node = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2),
    formula = c("formula_a", "formula_b", "formula_c")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # Check that drug names are sanitized
  expect_equal(result$drug[1], "drug_a")
  expect_equal(result$drug[2], "drug_b_c")
  expect_equal(result$drug[3], "drug_d_e")

  # Check that original names are preserved
  expect_equal(result$drug_name_original[1], "Drug A!")
  expect_equal(result$drug_name_original[2], "Drug-B/C")
  expect_equal(result$drug_name_original[3], "Drug.D E")
})

test_that("get_drugs_commands handles multiple drugs affecting same node", {

  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("target", "target", "other"),
    activity = c(0, 1, 2)
  )

  netw_variables <- tibble::tibble(
    node = c("target", "other"),
    id = c(10, 20),
    range_from = c(0, 0),
    range_to = c(2, 2),
    formula = c("target_formula", "other_formula")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # Check that both drugs targeting same node get same node info
  expect_equal(result$id[1:2], c(10, 10))
  expect_equal(result$command_arg[1], "-ko 10 0")
  expect_equal(result$command_arg[2], "-ko 10 1")
  expect_equal(result$command_arg[3], "-ko 20 2")
})

test_that("get_drugs_commands handles missing nodes in network", {

  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("existing", "missing"),
    activity = c(0, 1)
  )

  netw_variables <- tibble::tibble(
    node = c("existing"),
    id = c(1),
    range_from = c(0),
    range_to = c(2),
    formula = c("existing_formula")
  )

  expect_error(
    get_drugs_commands(drugs, netw_variables, "node"),
    "Column id contains NA values at positions: 2"
  )
})

test_that("get_drugs_commands works with custom node column names", {

  drugs <- tibble::tibble(
    drug = c("drug_a"),
    gene = c("test_gene"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    gene = c("test_gene"),
    id = c(5),
    range_from = c(0),
    range_to = c(2),
    formula = c("test_formula")
  )

  result <- get_drugs_commands(drugs, netw_variables, "gene")

  expect_equal(result$command_arg[1], "-ko 5 1")
  expect_true("gene" %in% colnames(result))
})

test_that("get_drugs_commands handles empty drugs data frame", {

  drugs <- tibble::tibble(
    drug = character(),
    node = character(),
    activity = numeric()
  )

  netw_variables <- tibble::tibble(
    node = c("a", "b"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2),
    formula = c("formula_a", "formula_b")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  expect_equal(nrow(result), 0)
  expected_cols <- c("drug", "node", "activity", "drug_name_original", "id",
                     "range_from", "range_to", "formula", "command_arg",
                     "alt_filename_part", "filename_part")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("get_drugs_commands handles empty network variables", {

  drugs <- tibble::tibble(
    drug = c("drug_a"),
    node = c("a"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    node = character(),
    id = integer(),
    range_from = numeric(),
    range_to = numeric(),
    formula = character()
  )

  expect_error(
    get_drugs_commands(drugs, netw_variables, "node"),
    "Column id contains NA values at positions: 1")
})

test_that("get_drugs_commands preserves additional network variable columns", {

  drugs <- tibble::tibble(
    drug = c("drug_a"),
    node = c("a"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    node = c("a"),
    id = c(1),
    range_from = c(0),
    range_to = c(2),
    formula = c("test_formula"),
    extra_col = c("extra_info")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  expect_true("extra_col" %in% colnames(result))
  expect_equal(result$extra_col[1], "extra_info")
})

test_that("get_drugs_commands generates correct alt_filename_part", {

  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("node_x", "node_y"),
    activity = c(0, 2)
  )

  netw_variables <- tibble::tibble(
    node = c("node_x", "node_y"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2),
    formula = c("formula_x", "formula_y")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # alt_filename_part should be based on make_command_args with node_col = "node"
  expect_equal(result$alt_filename_part[1], "node_x__0")
  expect_equal(result$alt_filename_part[2], "node_y__2")
})

test_that("get_drugs_commands handles duplicate drug names correctly", {

  drugs <- tibble::tibble(
    drug = c("Drug A", "Drug A", "Drug B"),
    node = c("node1", "node2", "node1"),
    activity = c(0, 1, 2)
  )

  netw_variables <- tibble::tibble(
    node = c("node1", "node2"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2),
    formula = c("formula1", "formula2")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # Should handle duplicate drug names by sanitizing each separately
  expect_equal(result$drug[1:2], c("drug_a", "drug_a"))
  expect_equal(result$drug_name_original[1:2], c("Drug A", "Drug A"))
})

test_that("get_drugs_commands command_arg format is correct", {

  drugs <- tibble::tibble(
    drug = c("test_drug"),
    node = c("test_node"),
    activity = c(2)
  )

  netw_variables <- tibble::tibble(
    node = c("test_node"),
    id = c(42),
    range_from = c(0),
    range_to = c(2),
    formula = c("test_formula")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # Command should be "-ko [id] [activity]"
  expect_equal(result$command_arg[1], "-ko 42 2")
})

test_that("get_drugs_commands filename_part format is correct", {

  drugs <- tibble::tibble(
    drug = c("Special-Drug!"),
    node = c("test_node"),
    activity = c(1)
  )

  netw_variables <- tibble::tibble(
    node = c("test_node"),
    id = c(1),
    range_from = c(0),
    range_to = c(2),
    formula = c("test_formula")
  )

  result <- get_drugs_commands(drugs, netw_variables, "node")

  # filename_part should be "[sanitized_drug]__NA"
  expect_equal(result$filename_part[1], "special_drug__NA")
})