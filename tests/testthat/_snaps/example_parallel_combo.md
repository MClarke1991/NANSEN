# example_parallel_combo.r runs without errors

    Code
      data
    Output
      # A tibble: 2,064 x 9
         filename             time    id    lo    hi node  range_from range_to formula
         <chr>               <dbl> <dbl> <dbl> <dbl> <chr>      <dbl>    <dbl> <lgl>  
       1 RAW__double__cance~     4     3     2     2 grow~          0        2 NA     
       2 RAW__double__cance~     4     4     0     0 a              0        2 NA     
       3 RAW__double__cance~     4     5     0     0 b              0        4 NA     
       4 RAW__double__cance~     4     6     2     2 outp~          0        2 NA     
       5 RAW__double__cance~     4     7     0     0 outp~          0        3 NA     
       6 RAW__double__cance~     4     9    10    10 c              0       10 NA     
       7 RAW__double__cance~     4    13     0     0 d              0        2 NA     
       8 RAW__double__cance~     4    19     0     0 e              0        1 NA     
       9 RAW__double__cance~     4     3     2     2 grow~          0        2 NA     
      10 RAW__double__cance~     4     4     0     0 a              0        2 NA     
      # i 2,054 more rows

---

    Code
      data
    Output
      # A tibble: 516 x 17
         case   background bkg_pert    muta   leva mutb   levb  time    id    lo    hi
         <chr>  <chr>      <chr>       <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
       1 double cancer     growth_fac~ a         0 b         0     4     6     2     2
       2 double cancer     growth_fac~ a         0 b         0     4     7     0     0
       3 double cancer     growth_fac~ a         0 b         4     4     6     0     0
       4 double cancer     growth_fac~ a         0 b         4     4     7     2     2
       5 double cancer     growth_fac~ a         0 c         0     7     6     0     0
       6 double cancer     growth_fac~ a         0 c         0     7     7     0     0
       7 double cancer     growth_fac~ a         0 c        10     7     6     2     2
       8 double cancer     growth_fac~ a         0 c        10     7     7     0     0
       9 double cancer     growth_fac~ a         0 d         0     7     6     2     2
      10 double cancer     growth_fac~ a         0 d         0     7     7     0     0
      # i 506 more rows
      # i 6 more variables: node <chr>, range_from <dbl>, range_to <dbl>,
      #   formula <lgl>, mean <dbl>, uncertainty <dbl>

---

    Code
      data
    Output
      # A tibble: 516 x 17
         case   background bkg_pert    muta   leva mutb   levb  time    id    lo    hi
         <chr>  <chr>      <chr>       <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
       1 double cancer     growth_fac~ a         0 b         0     4     6     2     2
       2 double cancer     growth_fac~ a         0 b         0     4     7     0     0
       3 double cancer     growth_fac~ a         0 b         4     4     6     0     0
       4 double cancer     growth_fac~ a         0 b         4     4     7     2     2
       5 double cancer     growth_fac~ a         0 c         0     7     6     0     0
       6 double cancer     growth_fac~ a         0 c         0     7     7     0     0
       7 double cancer     growth_fac~ a         0 c        10     7     6     2     2
       8 double cancer     growth_fac~ a         0 c        10     7     7     0     0
       9 double cancer     growth_fac~ a         0 d         0     7     6     2     2
      10 double cancer     growth_fac~ a         0 d         0     7     7     0     0
      # i 506 more rows
      # i 6 more variables: node <chr>, range_from <dbl>, range_to <dbl>,
      #   formula <lgl>, mean <dbl>, uncertainty <dbl>

