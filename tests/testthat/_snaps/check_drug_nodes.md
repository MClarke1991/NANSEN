# check_drug_nodes errors when drug nodes missing from network

    Code
      check_drug_nodes(drugs, netw_variables, "node")
    Condition
      Error in `check_drug_nodes()`:
      ! Nodes in drug list not in network:  [1] "missing_node"

# check_drug_nodes errors with multiple missing nodes

    Code
      check_drug_nodes(drugs, netw_variables, "node")
    Condition
      Error in `check_drug_nodes()`:
      ! Nodes in drug list not in network:  [1] "missing1" "missing2"

# check_drug_nodes handles empty network variables

    Code
      check_drug_nodes(drugs, netw_variables, "node")
    Condition
      Error in `check_drug_nodes()`:
      ! Nodes in drug list not in network:  [1] "a"

# check_drug_nodes handles case sensitivity

    Code
      check_drug_nodes(drugs, netw_variables, "node")
    Condition
      Error in `check_drug_nodes()`:
      ! Nodes in drug list not in network:  [1] "NodeA"

