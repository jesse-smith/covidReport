# `void(peds_table_race())` matches snapshot

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

# `peds_calc_race()` matches snapshot

    Code
      tbl_peds
    Output
      # A tibble: 4 x 4
        grp                        n percent    rate
        <fct>                  <int>   <dbl>   <dbl>
      1 Black/African American 12038  0.602   0.0861
      2 White                   5898  0.295   0.0697
      3 Other                   1014  0.0507  0.122 
      4 Missing                 1040  0.0520 NA     

