# check_perts_in_range errors when perturbations are below range

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        source experiment_particular gene  perturbation_bma range_from range_to
        <chr>  <chr>                 <chr>            <dbl>      <dbl>    <dbl>
      1 test1  exp1                  gene1               -1          0        3

# check_perts_in_range errors when perturbations are above range

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        source experiment_particular gene  perturbation_bma range_from range_to
        <chr>  <chr>                 <chr>            <dbl>      <dbl>    <dbl>
      1 test1  exp1                  gene1                4          0        3

# check_perts_in_range errors when expectations are below range

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        source experiment_particular gene  expectation_bma range_from range_to
        <chr>  <chr>                 <chr>           <dbl>      <dbl>    <dbl>
      1 test1  exp1                  gene1              -1          0        3

# check_perts_in_range errors when expectations are above range

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        source experiment_particular gene  expectation_bma range_from range_to
        <chr>  <chr>                 <chr>           <dbl>      <dbl>    <dbl>
      1 test1  exp1                  gene1               4          0        3

# check_perts_in_range errors with multiple out-of-range perturbations

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 2 x 6
        source experiment_particular gene  perturbation_bma range_from range_to
        <chr>  <chr>                 <chr>            <dbl>      <dbl>    <dbl>
      1 test1  exp1                  gene1               -1          0        3
      2 test2  exp2                  gene2                5          0        3

# check_perts_in_range errors with multiple out-of-range expectations

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 2 x 6
        source experiment_particular gene  expectation_bma range_from range_to
        <chr>  <chr>                 <chr>           <dbl>      <dbl>    <dbl>
      1 test1  exp1                  gene1              -1          0        3
      2 test2  exp2                  gene2               5          0        3

# check_perts_in_range handles mixed valid and invalid data

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        source experiment_particular gene  perturbation_bma range_from range_to
        <chr>  <chr>                 <chr>            <dbl>      <dbl>    <dbl>
      1 test2  exp2                  gene2               -1          0        3

# check_perts_in_range errors when gene has perturbation outside its specific range

    Code
      check_perts_in_range(spec_levels)
    Condition
      Error:
      ! There are perturbations that are outside the range the node can take: 
      # A tibble: 1 x 6
        source experiment_particular gene  perturbation_bma range_from range_to
        <chr>  <chr>                 <chr>            <dbl>      <dbl>    <dbl>
      1 test2  exp2                  gene2                3          0        2

