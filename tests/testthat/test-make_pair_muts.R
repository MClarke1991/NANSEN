source("testing_utils.r")

test_that("make_pair_muts creates correct pairwise mutations", {
  
  # Create test network variables with 3 nodes
  netw_variables <- tibble::tibble(
    name = c("a", "b", "c"),
    id = c(4, 5, 9),
    range_from = c(0, 0, 0),
    range_to = c(2, 4, 10)
  )
  
  result <- make_pair_muts(netw_variables)
  
  # With 3 nodes, we should have 3 choose 2 = 3 pairs
  # Each pair has 2 types (activation/inhibition) for each node = 4 combinations per pair
  # Total: 3 pairs * 4 combinations = 12 rows
  expect_equal(nrow(result), 12)
  
  # Check that required columns are present
  expected_cols <- c("a", "b", "id_a", "id_b", "type_a", "type_b", 
                     "activity_a", "activity_b", "command_arg", "filename_part")
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check that all pairs are represented
  pairs <- unique(paste(result$a, result$b, sep = "_"))
  expect_true("a_b" %in% pairs)
  expect_true("a_c" %in% pairs)
  expect_true("b_c" %in% pairs)
  
  # No self-pairs should exist
  expect_false(any(result$a == result$b))
})

test_that("make_pair_muts command arguments are correctly formatted", {
  
  netw_variables <- tibble::tibble(
    name = c("a", "b"),
    id = c(4, 5),
    range_from = c(0, 0),
    range_to = c(2, 4)
  )
  
  result <- make_pair_muts(netw_variables)
  
  # Should have 1 pair with 4 combinations
  expect_equal(nrow(result), 4)
  
  # Check command format: "-ko id_a activity_a -ko id_b activity_b"
  for (i in 1:nrow(result)) {
    expected_cmd <- paste("-ko", result$id_a[i], result$activity_a[i],
                         "-ko", result$id_b[i], result$activity_b[i])
    expect_equal(result$command_arg[i], expected_cmd)
  }
  
  # Check filename format: "a__activity_a__b__activity_b"
  for (i in 1:nrow(result)) {
    expected_filename <- paste(result$a[i], result$activity_a[i], 
                              result$b[i], result$activity_b[i], sep = "__")
    expect_equal(result$filename_part[i], expected_filename)
  }
})

test_that("make_pair_muts handles activation and inhibition combinations", {
  
  netw_variables <- tibble::tibble(
    name = c("a", "b"),
    id = c(1, 2),
    range_from = c(0, 1),
    range_to = c(2, 3)
  )
  
  result <- make_pair_muts(netw_variables)
  
  # Should have all 4 combinations: activ-activ, activ-inhib, inhib-activ, inhib-inhib
  type_combinations <- paste(result$type_a, result$type_b, sep = "_")
  expect_true("activation_activation" %in% type_combinations)
  expect_true("activation_inhibition" %in% type_combinations)
  expect_true("inhibition_activation" %in% type_combinations)
  expect_true("inhibition_inhibition" %in% type_combinations)
  
  # Check that activation uses range_to and inhibition uses range_from
  activ_a <- result[result$type_a == "activation", ]
  expect_true(all(activ_a$activity_a == 2))  # range_to for node a
  
  inhib_a <- result[result$type_a == "inhibition", ]
  expect_true(all(inhib_a$activity_a == 0))  # range_from for node a
  
  activ_b <- result[result$type_b == "activation", ]
  expect_true(all(activ_b$activity_b == 3))  # range_to for node b
  
  inhib_b <- result[result$type_b == "inhibition", ]
  expect_true(all(inhib_b$activity_b == 1))  # range_from for node b
})

test_that("make_pair_muts works with custom node column", {
  
  netw_variables <- tibble::tibble(
    gene = c("x", "y"),
    id = c(10, 20),
    range_from = c(0, 0),
    range_to = c(1, 1)
  )
  
  result <- make_pair_muts(netw_variables, node_col = "gene")
  
  # Should work with gene column instead of name
  expect_equal(nrow(result), 4)
  expect_true(all(result$a == "x"))
  expect_true(all(result$b == "y"))
  expect_true(all(result$id_a == 10))
  expect_true(all(result$id_b == 20))
})

test_that("make_pair_muts handles single node (no pairs possible)", {
  
  netw_variables <- tibble::tibble(
    name = c("only_node"),
    id = c(1),
    range_from = c(0),
    range_to = c(2)
  )
  
  result <- make_pair_muts(netw_variables)
  
  # With only 1 node, no pairs are possible
  expect_equal(nrow(result), 0)
  
  # But should still have the correct column structure
  expected_cols <- c("a", "b", "id_a", "id_b", "type_a", "type_b",
                     "activity_a", "activity_b", "command_arg", "filename_part")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("make_pair_muts handles empty network variables", {
  
  netw_variables <- tibble::tibble(
    name = character(),
    id = integer(),
    range_from = numeric(),
    range_to = numeric()
  )
  
  result <- make_pair_muts(netw_variables)
  
  # Should return empty data frame with correct structure
  expect_equal(nrow(result), 0)
  expected_cols <- c("a", "b", "id_a", "id_b", "type_a", "type_b",
                     "activity_a", "activity_b", "command_arg", "filename_part")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("make_pair_muts produces correct number of combinations", {
  
  # Test with different numbers of nodes
  for (n_nodes in 2:4) {
    node_names <- letters[1:n_nodes]
    netw_variables <- tibble::tibble(
      name = node_names,
      id = 1:n_nodes,
      range_from = rep(0, n_nodes),
      range_to = rep(2, n_nodes)
    )
    
    result <- make_pair_muts(netw_variables)
    
    # Each pair has 4 type combinations (2 types * 2 types)
    expected_pairs <- choose(n_nodes, 2)
    expected_rows <- expected_pairs * 4
    
    expect_equal(nrow(result), expected_rows,
                 info = paste("Failed for", n_nodes, "nodes"))
  }
})

test_that("make_pair_muts maintains proper join relationships", {
  
  netw_variables <- tibble::tibble(
    name = c("node1", "node2", "node3"),
    id = c(10, 20, 30),
    range_from = c(1, 2, 3),
    range_to = c(5, 6, 7)
  )
  
  result <- make_pair_muts(netw_variables)
  
  # Check that joins worked correctly - each row should have consistent node info
  for (i in 1:nrow(result)) {
    # Node a info should be consistent
    node_a_info <- netw_variables[netw_variables$name == result$a[i], ]
    expect_equal(result$id_a[i], node_a_info$id)
    
    # Node b info should be consistent  
    node_b_info <- netw_variables[netw_variables$name == result$b[i], ]
    expect_equal(result$id_b[i], node_b_info$id)
    
    # Activity should match type and range
    if (result$type_a[i] == "activation") {
      expect_equal(result$activity_a[i], node_a_info$range_to)
    } else {
      expect_equal(result$activity_a[i], node_a_info$range_from)
    }
    
    if (result$type_b[i] == "activation") {
      expect_equal(result$activity_b[i], node_b_info$range_to)
    } else {
      expect_equal(result$activity_b[i], node_b_info$range_from)
    }
  }
})