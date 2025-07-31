# stop_missing_nodes_perturbed errors when perturbed nodes missing from network

    Code
      stop_missing_nodes_perturbed(spec = spec, missing_nodes_perturbed_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 2 Nodes which are perturbed in the specification are missing from the provided BMA network: 
      missing_node1
      missing_node2

# stop_missing_nodes_perturbed handles mixed valid and invalid nodes

    Code
      stop_missing_nodes_perturbed(spec = spec, missing_nodes_perturbed_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 2 Nodes which are perturbed in the specification are missing from the provided BMA network: 
      missing_node
      another_missing

# stop_missing_nodes_perturbed handles duplicate perturbed genes

    Code
      stop_missing_nodes_perturbed(spec = spec, missing_nodes_perturbed_overide = FALSE,
        netw_variables = netw_variables, log_file = log_file)
    Condition
      Error:
      ! 1 Nodes which are perturbed in the specification are missing from the provided BMA network: 
      missing_node

