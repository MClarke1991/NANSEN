# combo integration test - Windows only

    Code
      processed_data
    Output
      # A tibble: 2,224 x 17
         case   background bkg_pert    muta   leva mutb   levb  time    id    lo    hi
         <chr>  <chr>      <chr>       <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
       1 double cancer     growth_fac~ a         0 b         0     4     3     2     2
       2 double cancer     growth_fac~ a         0 b         0     4     4     0     0
       3 double cancer     growth_fac~ a         0 b         0     4     5     0     0
       4 double cancer     growth_fac~ a         0 b         0     4     6     2     2
       5 double cancer     growth_fac~ a         0 b         0     4     7     0     0
       6 double cancer     growth_fac~ a         0 b         0     4     9    10    10
       7 double cancer     growth_fac~ a         0 b         0     4    13     0     0
       8 double cancer     growth_fac~ a         0 b         0     4    19     0     0
       9 double cancer     growth_fac~ a         0 b         4     4     3     2     2
      10 double cancer     growth_fac~ a         0 b         4     4     4     0     0
      # i 2,214 more rows
      # i 6 more variables: node <chr>, range_from <dbl>, range_to <dbl>,
      #   formula <chr>, mean <dbl>, uncertainty <dbl>

