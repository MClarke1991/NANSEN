# check_drugs_in_range errors when activity below range_from

    Code
      check_drugs_in_range(drugs_commands)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        drug   node  activity range_from range_to command_arg
        <chr>  <chr>    <dbl>      <dbl>    <dbl> <chr>      
      1 drug_a a           -1          0        2 -ko 1 -1   

# check_drugs_in_range errors when activity above range_to

    Code
      check_drugs_in_range(drugs_commands)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        drug   node  activity range_from range_to command_arg
        <chr>  <chr>    <dbl>      <dbl>    <dbl> <chr>      
      1 drug_b b            5          0        2 -ko 2 5    

# check_drugs_in_range errors with multiple out-of-range activities

    Code
      check_drugs_in_range(drugs_commands)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 2 x 6
        drug   node  activity range_from range_to command_arg
        <chr>  <chr>    <dbl>      <dbl>    <dbl> <chr>      
      1 drug_a a           -1          0        2 -ko 1 -1   
      2 drug_c c           10          0        5 -ko 3 10   

# check_drugs_in_range handles decimal activity values

    Code
      check_drugs_in_range(drugs_commands_bad)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        drug   node  activity range_from range_to command_arg
        <chr>  <chr>    <dbl>      <dbl>    <dbl> <chr>      
      1 drug_a a          2.1          0        2 -ko 1 2.1  

# check_drugs_in_range handles mixed valid and invalid activities

    Code
      check_drugs_in_range(drugs_commands)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 2 x 6
        drug          node  activity range_from range_to command_arg
        <chr>         <chr>    <dbl>      <dbl>    <dbl> <chr>      
      1 bad_drug_low  b           -1          0        2 -ko 2 -1   
      2 bad_drug_high c           10          0        5 -ko 3 10   

# check_drugs_in_range works with identical ranges

    Code
      check_drugs_in_range(drugs_commands_bad)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        drug       node       activity range_from range_to command_arg
        <chr>      <chr>         <dbl>      <dbl>    <dbl> <chr>      
      1 fixed_drug fixed_node        2          1        1 -ko 1 2    

