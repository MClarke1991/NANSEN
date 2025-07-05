test_that("node_name_check accepts valid node names", {
  expect_silent(node_name_check("ValidNode"))
  expect_silent(node_name_check("gene1"))
  expect_silent(node_name_check("p53"))
  expect_silent(node_name_check("BRCA1"))
  expect_silent(node_name_check("node_with_underscore"))
  expect_silent(node_name_check("node123"))
  expect_silent(node_name_check("a"))
  expect_silent(node_name_check("Very_Long_Node_Name_123"))
})

test_that("node_name_check rejects names with spaces", {
  expect_error(node_name_check("node with space"), 
               "Error: Node names cannot contain spaces")
  expect_error(node_name_check(" leading_space"), 
               "Error: Node names cannot contain spaces")
  expect_error(node_name_check("trailing_space "), 
               "Error: Node names cannot contain spaces")
  expect_error(node_name_check("multiple spaces here"), 
               "Error: Node names cannot contain spaces")
})

test_that("node_name_check rejects names with double underscores", {
  expect_error(node_name_check("node__with__double"), 
               "Error: Node names cannot contain double underscores")
  expect_error(node_name_check("__leading_double"), 
               "Error: Node names cannot contain double underscores")
  expect_error(node_name_check("trailing__"), 
               "Error: Node names cannot contain double underscores")
  expect_error(node_name_check("a__b"), 
               "Error: Node names cannot contain double underscores")
})

test_that("node_name_check rejects names containing 'Mut'", {
  expect_error(node_name_check("Mut"), 
               "Error: Node names cannot contain 'Mut', see node: 'Mut'")
  expect_error(node_name_check("MutNode"), 
               "Error: Node names cannot contain 'Mut', see node: 'MutNode'")
  expect_error(node_name_check("NodeMut"), 
               "Error: Node names cannot contain 'Mut', see node: 'NodeMut'")
  expect_error(node_name_check("Node_Mut_Test"), 
               "Error: Node names cannot contain 'Mut', see node: 'Node_Mut_Test'")
  expect_error(node_name_check("Mutation"), 
               "Error: Node names cannot contain 'Mut', see node: 'Mutation'")
})

test_that("node_name_check rejects names containing '_cex'", {
  expect_error(node_name_check("_cex"), 
               "Error: Node names cannot contain '_cex'")
  expect_error(node_name_check("node_cex"), 
               "Error: Node names cannot contain '_cex'")
  expect_error(node_name_check("node_cex_test"), 
               "Error: Node names cannot contain '_cex'")
  expect_error(node_name_check("test_cex"), 
               "Error: Node names cannot contain '_cex'")
})

test_that("node_name_check rejects names containing '.json'", {
  expect_error(node_name_check(".json"), 
               "Error: Node names cannot contain '.json'")
  expect_error(node_name_check("file.json"), 
               "Error: Node names cannot contain '.json'")
  expect_error(node_name_check("data.json.backup"), 
               "Error: Node names cannot contain '.json'")
  expect_error(node_name_check("test.json.old"), 
               "Error: Node names cannot contain '.json'")
})

test_that("node_name_check rejects names containing 'PERT'", {
  expect_snapshot(node_name_check("PERT"), error = TRUE)
  expect_snapshot(node_name_check("PERTNode"), error = TRUE)
  expect_snapshot(node_name_check("NodePERT"), error = TRUE)
  expect_snapshot(node_name_check("Node_PERT_Test"), error = TRUE)
  expect_snapshot(node_name_check("PERTurbation"), error = TRUE)
})

test_that("node_name_check handles edge cases", {
  # Empty string should be valid (no forbidden patterns)
  expect_silent(node_name_check(""))
  
  # Single characters that aren't forbidden
  expect_silent(node_name_check("_"))
  expect_silent(node_name_check("1"))
  expect_silent(node_name_check("."))
  
  # Names with single underscores are valid
  expect_silent(node_name_check("node_1"))
  expect_silent(node_name_check("_node"))
  expect_silent(node_name_check("node_"))
  
  # Case sensitivity checks
  expect_silent(node_name_check("mut"))  # lowercase should be valid
  expect_silent(node_name_check("pert"))  # lowercase should be valid
  expect_silent(node_name_check("JSON"))  # uppercase should be valid
})

test_that("node_name_check handles multiple violations", {
  # Node with multiple violations should stop at first one found
  expect_snapshot(node_name_check("node with space__double"), error = TRUE)
  expect_snapshot(node_name_check("Mut__double"), error = TRUE)
  expect_snapshot(node_name_check("PERT.json"), error = TRUE)
})