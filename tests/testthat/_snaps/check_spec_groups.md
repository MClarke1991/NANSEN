# check_spec_groups errors on non-consecutive duplicates

    Code
      check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))
    Condition
      Error:
      ! Unique experiments are expected to be grouped such that they are in a block of consecutive rows, with a unique combination of the columns 'source', 'cell_line' and 'experiment_particular'. In the provided specification the combination of these columns repeats in non-consecutive blocks of rows, suggesting non-unique descriptions of separate experiments, or a single experiment that has been split. Please move all rows of a single experiment to be together, or rename these columns such that different experiments are uniquely identified. The repeated descriptions are:
      # A tibble: 1 x 5
        value dupe_count source cell_line experiment_particular
        <int>      <int> <chr>  <chr>     <chr>                
      1     2          2 exp2   cellB     treatment2           

# check_spec_groups errors on multiple scattered experiments

    Code
      check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))
    Condition
      Error:
      ! Unique experiments are expected to be grouped such that they are in a block of consecutive rows, with a unique combination of the columns 'source', 'cell_line' and 'experiment_particular'. In the provided specification the combination of these columns repeats in non-consecutive blocks of rows, suggesting non-unique descriptions of separate experiments, or a single experiment that has been split. Please move all rows of a single experiment to be together, or rename these columns such that different experiments are uniquely identified. The repeated descriptions are:
      # A tibble: 1 x 5
        value dupe_count source cell_line experiment_particular
        <int>      <int> <chr>  <chr>     <chr>                
      1     2          2 exp2   cellB     treatment2           

# check_spec_groups errors on interleaved experiments

    Code
      check_spec_groups(spec, c("source", "cell_line", "experiment_particular"))
    Condition
      Error:
      ! Unique experiments are expected to be grouped such that they are in a block of consecutive rows, with a unique combination of the columns 'source', 'cell_line' and 'experiment_particular'. In the provided specification the combination of these columns repeats in non-consecutive blocks of rows, suggesting non-unique descriptions of separate experiments, or a single experiment that has been split. Please move all rows of a single experiment to be together, or rename these columns such that different experiments are uniquely identified. The repeated descriptions are:
      # A tibble: 2 x 5
        value dupe_count source cell_line experiment_particular
        <int>      <int> <chr>  <chr>     <chr>                
      1     1          2 exp1   cellA     treatment1           
      2     2          2 exp2   cellB     treatment2           

