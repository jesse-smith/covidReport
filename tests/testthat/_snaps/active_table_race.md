# `void(active_table_race())` matches snapshot

    Code
      tbl_void
    Output
      a flextable object.
      col_keys: `grp`, `n`, `percent`, `rate` 
      header has 1 row(s) 
      body has 5 row(s) 
      original dataset sample: 
                           grp    n percent  rate
      1 Black/African American  500      50 10000
      2                  White  300      30 10000
      3                  Other  100      10 10000
      4                Missing  100      10 10000
      5                  Total 1000     100    NA

# `active_calc_race()` matches snapshot

    Code
      tbl_active
    Output
      # A tibble: 4 x 4
        grp                        n percent      rate
        <fct>                  <int>   <dbl>     <dbl>
      1 Black/African American   547  0.608   0.00106 
      2 White                    259  0.288   0.000666
      3 Other                     50  0.0556  0.00154 
      4 Missing                   44  0.0489 NA       

