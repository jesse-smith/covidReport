# `void(death_table_ethnicity())` matches snapshot

    Code
      tbl_void
    Output
      a flextable object.
      col_keys: `grp`, `n`, `percent`, `rate` 
      header has 1 row(s) 
      body has 4 row(s) 
      original dataset sample: 
                        grp    n percent       rate
      1     Hispanic/Latino  700      70 10000.0000
      2 Not Hispanic/Latino  100      10 10000.0000
      3             Missing  200      20         NA
      4               Total 1000     100   106.7047

# `death_calc_ethnicity()` matches snapshot

    Code
      tbl_death
    Output
      # A tibble: 3 x 4
        grp                     n percent     rate
        <fct>               <int>   <dbl>    <dbl>
      1 Hispanic/Latino      6984   0.698  0.112  
      2 Not Hispanic/Latino  1017   0.102  0.00116
      3 Missing              1998   0.200 NA      

