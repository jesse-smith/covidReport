# `void(peds_table_sex())` matches snapshot

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

# `peds_calc_sex()` matches snapshot

    Code
      tbl_peds
    Output
      # A tibble: 3 x 4
        grp         n percent    rate
        <fct>   <int>   <dbl>   <dbl>
      1 Female  10064  0.503   0.0877
      2 Male     9692  0.485   0.0821
      3 Missing   234  0.0117 NA     

