test_that("make_single_muts works with valid network variables", {
  # Create test network variables
  netw_variables <- data.frame(
    name = c("node1", "node2", "node3"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 1),
    range_to = c(2, 1, 3),
    formula = c("", "", ""),
    stringsAsFactors = FALSE
  )

  result <- make_single_muts(netw_variables)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("name", "id", "type", "activity", "command_arg", "filename_part") %in% names(result)))

  # Should have 2 rows per node (activation + inhibition) plus 1 baseline = 7 rows
  expect_equal(nrow(result), 7)

  # Check baseline case
  baseline_row <- result[result$name == "baseline", ]
  expect_equal(nrow(baseline_row), 1)
  expect_equal(baseline_row$command_arg, "")
  expect_equal(baseline_row$filename_part, "baseline")
  expect_true(is.na(baseline_row$id))
  expect_true(is.na(baseline_row$activity))
})

test_that("make_single_muts generates correct activation and inhibition pairs", {
  netw_variables <- data.frame(
    name = c("geneA", "geneB"),
    id = c(10, 20),
    range_from = c(0, 1),
    range_to = c(3, 4),
    formula = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- make_single_muts(netw_variables)

  # Filter out baseline
  mut_rows <- result[result$name != "baseline", ]

  # Should have activation and inhibition for each gene
  expect_true("activation" %in% mut_rows$type)
  expect_true("inhibition" %in% mut_rows$type)

  # Check geneA activation (range_to = 3)
  geneA_activation <- mut_rows[mut_rows$name == "geneA" & mut_rows$type == "activation", ]
  expect_equal(geneA_activation$activity, 3)
  expect_equal(geneA_activation$command_arg, "-ko 10 3")
  expect_equal(geneA_activation$filename_part, "geneA__3")

  # Check geneA inhibition (range_from = 0)
  geneA_inhibition <- mut_rows[mut_rows$name == "geneA" & mut_rows$type == "inhibition", ]
  expect_equal(geneA_inhibition$activity, 0)
  expect_equal(geneA_inhibition$command_arg, "-ko 10 0")
  expect_equal(geneA_inhibition$filename_part, "geneA__0")

  # Check geneB activation (range_to = 4)
  geneB_activation <- mut_rows[mut_rows$name == "geneB" & mut_rows$type == "activation", ]
  expect_equal(geneB_activation$activity, 4)
  expect_equal(geneB_activation$command_arg, "-ko 20 4")
  expect_equal(geneB_activation$filename_part, "geneB__4")

  # Check geneB inhibition (range_from = 1)
  geneB_inhibition <- mut_rows[mut_rows$name == "geneB" & mut_rows$type == "inhibition", ]
  expect_equal(geneB_inhibition$activity, 1)
  expect_equal(geneB_inhibition$command_arg, "-ko 20 1")
  expect_equal(geneB_inhibition$filename_part, "geneB__1")
})

test_that("make_single_muts works with custom node_col", {
  # Use custom column name for nodes
  netw_variables <- data.frame(
    gene_name = c("custom1", "custom2"),
    id = c(5, 6),
    range_from = c(0, 0),
    range_to = c(1, 2),
    formula = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- make_single_muts(netw_variables, node_col = "gene_name")

  expect_true("gene_name" %in% names(result))
  expect_true("custom1" %in% result$gene_name)
  expect_true("custom2" %in% result$gene_name)
  expect_true("baseline" %in% result$gene_name)

  # Check that filename_part uses gene_name
  custom1_row <- result[result$gene_name == "custom1" & result$type == "activation", ]
  expect_equal(custom1_row$filename_part, "custom1__1")
})

test_that("make_single_muts handles missing node_col", {
  netw_variables <- data.frame(
    wrong_name = c("node1"),
    id = c(1),
    range_from = c(0),
    range_to = c(1),
    formula = c(""),
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    make_single_muts(netw_variables),
    error = TRUE
  )
})

test_that("make_single_muts handles empty input", {
  # Empty but correctly structured dataframe
  netw_variables <- data.frame(
    name = character(0),
    id = integer(0),
    range_from = integer(0),
    range_to = integer(0),
    formula = character(0),
    stringsAsFactors = FALSE
  )

  result <- make_single_muts(netw_variables)

  # Should still have baseline case
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "baseline")
  expect_equal(result$command_arg, "")
  expect_equal(result$filename_part, "baseline")
})

test_that("make_single_muts handles single node correctly", {
  netw_variables <- data.frame(
    name = c("single_node"),
    id = c(99),
    range_from = c(1),
    range_to = c(5),
    formula = c(""),
    stringsAsFactors = FALSE
  )

  result <- make_single_muts(netw_variables)

  # Should have 2 mutation rows + 1 baseline = 3 total
  expect_equal(nrow(result), 3)

  # Check both mutations are present
  mut_rows <- result[result$name == "single_node", ]
  expect_equal(nrow(mut_rows), 2)
  expect_true(all(c("activation", "inhibition") %in% mut_rows$type))
})

test_that("make_single_muts works with example network data", {
  # Use get_netw_variables to get real network data
  netw_file <- here::here("examples", "autopert", "helper_autopert_1.json")
  netw_variables <- get_netw_variables(netw_file)

  result <- make_single_muts(netw_variables)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Should have baseline plus 2 rows per node in network
  expected_rows <- (nrow(netw_variables) * 2) + 1
  expect_equal(nrow(result), expected_rows)

  # Verify baseline is present
  expect_true("baseline" %in% result$name)

  # Verify all nodes from network are represented
  network_nodes <- unique(netw_variables$name)
  result_nodes <- unique(result$name[result$name != "baseline"])
  expect_true(all(network_nodes %in% result_nodes))
})

test_that("make_single_muts handles nodes with same range_from and range_to", {
  # This tests edge case where inhibition and activation are the same
  netw_variables <- data.frame(
    name = c("fixed_node"),
    id = c(1),
    range_from = c(2),
    range_to = c(2),  # Same as range_from
    formula = c(""),
    stringsAsFactors = FALSE
  )

  result <- make_single_muts(netw_variables)

  # Should still create both activation and inhibition rows
  mut_rows <- result[result$name == "fixed_node", ]
  expect_equal(nrow(mut_rows), 2)

  # Both should have activity = 2
  expect_true(all(mut_rows$activity == 2))
  expect_true(all(mut_rows$command_arg == "-ko 1 2"))
})