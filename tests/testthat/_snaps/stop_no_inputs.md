# stop_no_inputs errors when experiment has all NA perturbations

    Code
      stop_no_inputs(spec, log_file, group_vars)
    Condition
      Error:
      ! There are cases where there are no perturbations of the network, only
            measurement of the basal output. Currently (2020-02-26) this will cause
            an error. Normally would expect `n_total` an experiment to be
            greater than `n_blank_input` which are usually nodes where one is
            measuring the output.
            In spec there are: 
      # A tibble: 3 x 6
        source cell_line experiment_particular n_blank_input n_total csv_row_id
        <chr>  <chr>     <chr>                         <int>   <int>      <dbl>
      1 exp1   cellA     treatment1                        3       3          2
      2 exp1   cellA     treatment1                        3       3          3
      3 exp1   cellA     treatment1                        3       3          4

# stop_no_inputs errors when multiple experiments have all NA perturbations

    Code
      stop_no_inputs(spec, log_file, group_vars)
    Condition
      Error:
      ! There are cases where there are no perturbations of the network, only
            measurement of the basal output. Currently (2020-02-26) this will cause
            an error. Normally would expect `n_total` an experiment to be
            greater than `n_blank_input` which are usually nodes where one is
            measuring the output.
            In spec there are: 
      # A tibble: 4 x 6
        source cell_line experiment_particular n_blank_input n_total csv_row_id
        <chr>  <chr>     <chr>                         <int>   <int>      <dbl>
      1 exp1   cellA     treatment1                        2       2          2
      2 exp1   cellA     treatment1                        2       2          3
      3 exp3   cellC     treatment3                        2       2          6
      4 exp3   cellC     treatment3                        2       2          7

# stop_no_inputs errors when single row experiment has NA perturbation

    Code
      stop_no_inputs(spec, log_file, group_vars)
    Condition
      Error:
      ! There are cases where there are no perturbations of the network, only
            measurement of the basal output. Currently (2020-02-26) this will cause
            an error. Normally would expect `n_total` an experiment to be
            greater than `n_blank_input` which are usually nodes where one is
            measuring the output.
            In spec there are: 
      # A tibble: 1 x 6
        source cell_line experiment_particular n_blank_input n_total csv_row_id
        <chr>  <chr>     <chr>                         <int>   <int>      <dbl>
      1 exp1   cellA     treatment1                        1       1          2

# stop_no_inputs errors with different group_vars when experiment has all NA

    Code
      stop_no_inputs(spec, log_file, group_vars)
    Condition
      Error:
      ! There are cases where there are no perturbations of the network, only
            measurement of the basal output. Currently (2020-02-26) this will cause
            an error. Normally would expect `n_total` an experiment to be
            greater than `n_blank_input` which are usually nodes where one is
            measuring the output.
            In spec there are: 
      # A tibble: 2 x 5
        source batch  n_blank_input n_total csv_row_id
        <chr>  <chr>          <int>   <int>      <dbl>
      1 exp1   batch1             2       2          2
      2 exp1   batch1             2       2          3

