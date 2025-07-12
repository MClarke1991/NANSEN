# stop_missing_nodes_expected errors when expected nodes missing from network

    Code
      stop_missing_nodes_expected(spec = spec, missing_nodes_expected_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 2 nodes, which were measured in the experiments, for which there is an expected result, in the specification are missing from the provided BMA network: 
      missing_expected1,
      missing_expected2

# stop_missing_nodes_expected issues warning with override enabled

    Code
      stop_missing_nodes_expected(spec = spec, missing_nodes_expected_overide = TRUE,
        netw_variables = netw_variables, log_file = log_file)

# stop_missing_nodes_expected handles mixed valid and invalid nodes

    Code
      stop_missing_nodes_expected(spec = spec, missing_nodes_expected_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 2 nodes, which were measured in the experiments, for which there is an expected result, in the specification are missing from the provided BMA network: 
      missing_expected,
      another_missing

# stop_missing_nodes_expected handles duplicate expected genes

    Code
      stop_missing_nodes_expected(spec = spec, missing_nodes_expected_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 1 nodes, which were measured in the experiments, for which there is an expected result, in the specification are missing from the provided BMA network: 
      missing_expected

# stop_missing_nodes_expected handles nodes that are both perturbed and expected

    Code
      stop_missing_nodes_expected(spec = spec, missing_nodes_expected_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 1 nodes, which were measured in the experiments, for which there is an expected result, in the specification are missing from the provided BMA network: 
      missing_combo

# stop_missing_nodes_expected message format is correct

    Code
      stop_missing_nodes_expected(spec = spec, missing_nodes_expected_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 2 nodes, which were measured in the experiments, for which there is an expected result, in the specification are missing from the provided BMA network: 
      missing_A,
      missing_B

