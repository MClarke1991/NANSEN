# combo_parallel integration test - Windows only

    Code
      processed_integrated
    Output
      # A tibble: 2,064 x 17
         case   background bkg_pert    muta   leva mutb   levb  time    id    lo    hi
         <chr>  <chr>      <chr>       <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
       1 double wt         growth_fac~ a         0 b         0     5     3     2     2
       2 double wt         growth_fac~ a         0 b         0     5     4     0     0
       3 double wt         growth_fac~ a         0 b         0     5     5     0     0
       4 double wt         growth_fac~ a         0 b         0     5     6     2     2
       5 double wt         growth_fac~ a         0 b         0     5     7     0     0
       6 double wt         growth_fac~ a         0 b         0     5     9    10    10
       7 double wt         growth_fac~ a         0 b         0     5    13     0     0
       8 double wt         growth_fac~ a         0 b         0     5    19     0     0
       9 double wt         growth_fac~ a         0 b         4     5     3     2     2
      10 double wt         growth_fac~ a         0 b         4     5     4     0     0
      # i 2,054 more rows
      # i 6 more variables: node <chr>, range_from <dbl>, range_to <dbl>,
      #   formula <chr>, mean <dbl>, uncertainty <dbl>

# combo_parallel with short_filenames = TRUE integration test - Windows only

    Code
      processed_integrated
    Output
      # A tibble: 4,128 x 18
         case   background bkg_pert    muta   leva mutb   levb  time    id    lo    hi
         <chr>  <chr>      <chr>       <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
       1 double wt         growth_fac~ a         0 b         0     5     3     2     2
       2 double wt         growth_fac~ a         0 b         0     5     4     0     0
       3 double wt         growth_fac~ a         0 b         0     5     5     0     0
       4 double wt         growth_fac~ a         0 b         0     5     6     2     2
       5 double wt         growth_fac~ a         0 b         0     5     7     0     0
       6 double wt         growth_fac~ a         0 b         0     5     9    10    10
       7 double wt         growth_fac~ a         0 b         0     5    13     0     0
       8 double wt         growth_fac~ a         0 b         0     5    19     0     0
       9 double wt         growth_fac~ a         0 b         4     5     3     2     2
      10 double wt         growth_fac~ a         0 b         4     5     4     0     0
      # i 4,118 more rows
      # i 7 more variables: node <chr>, range_from <dbl>, range_to <dbl>,
      #   formula <chr>, file <lgl>, mean <dbl>, uncertainty <dbl>

