# `void(death_table_race())` matches snapshot

    Code
      tbl_void
    Output
      a flextable object.
      col_keys: `grp`, `n`, `percent`, `rate` 
      header has 1 row(s) 
      body has 5 row(s) 
      original dataset sample: 
                           grp    n percent       rate
      1 Black/African American  500      50 10000.0000
      2                  White  300      30 10000.0000
      3                  Other  100      10 10000.0000
      4                Missing  100      10 10000.0000
      5                  Total 1000     100   106.7047

# `death_calc_race()` matches snapshot

    Code
      tbl_death
    Output
      # A tibble: 4 x 4
        grp                        n percent     rate
        <fct>                  <int>   <dbl>    <dbl>
      1 Black/African American  5964  0.596   0.0116 
      2 White                   3054  0.305   0.00785
      3 Other                    509  0.0509 NA      
      4 Missing                  472  0.0472 NA      

