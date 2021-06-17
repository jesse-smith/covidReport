# `void(death_table_sex())` matches snapshot

    Code
      tbl_void
    Output
      a flextable object.
      col_keys: `grp`, `n`, `percent`, `rate` 
      header has 1 row(s) 
      body has 4 row(s) 
      original dataset sample: 
            grp    n percent       rate
      1  Female  500      50 10000.0000
      2    Male  400      40 10000.0000
      3 Missing  100      10 10000.0000
      4   Total 1000     100   106.7047

# `death_calc_sex()` matches snapshot

    Code
      tbl_death
    Output
      # A tibble: 3 x 4
        grp         n percent    rate
        <fct>   <int>   <dbl>   <dbl>
      1 Female   4989  0.499   0.0101
      2 Male     4900  0.490   0.0110
      3 Missing   110  0.0110 NA     

