# get_netw_variables handles missing file

    Code
      get_netw_variables("nonexistent_file.json")
    Condition
      Error:
      ! lexical error: invalid string in json text.
                                             nonexistent_file.json
                           (right here) ------^

# get_netw_variables detects duplicate node names

    Code
      get_netw_variables(temp_json)
    Condition
      Error in `get_netw_variables()`:
      ! Node names must be unique

# get_netw_variables detects zero granularity nodes

    Code
      get_netw_variables(temp_json)
    Condition
      Error in `get_netw_variables()`:
      ! Nodes with zero granularity (max same as min) are often a mistake and cannot be perturbed. Check:
      zero_gran

# get_netw_variables handles malformed JSON

    Code
      get_netw_variables(temp_json)
    Condition
      Error in `parse_con()`:
      ! lexical error: invalid char in json text.
                     {"Model":{"Variables":[{malformed}]}}  
                           (right here) ------^

# get_netw_variables handles missing Model structure

    Code
      get_netw_variables(temp_json)
    Condition
      Error:
      ! `clean_names()` requires that either names or dimnames be non-null.

