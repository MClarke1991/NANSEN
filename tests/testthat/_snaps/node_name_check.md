# node_name_check rejects names containing 'PERT'

    Code
      node_name_check("PERT")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain 'PERT', see node: 'PERT'

---

    Code
      node_name_check("PERTNode")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain 'PERT', see node: 'PERTNode'

---

    Code
      node_name_check("NodePERT")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain 'PERT', see node: 'NodePERT'

---

    Code
      node_name_check("Node_PERT_Test")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain 'PERT', see node: 'Node_PERT_Test'

---

    Code
      node_name_check("PERTurbation")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain 'PERT', see node: 'PERTurbation'

# node_name_check handles multiple violations

    Code
      node_name_check("node with space__double")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain spaces, see node: 'node with space__double'

---

    Code
      node_name_check("Mut__double")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain double underscores (__), see node: 'Mut__double'

---

    Code
      node_name_check("PERT.json")
    Condition
      Error in `node_name_check()`:
      ! Error: Node names cannot contain '.json', see node: 'PERT.json'

