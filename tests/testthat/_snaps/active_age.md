# `void(active_table_age())` matches snapshot

    Code
      tbl_void
    Output
      a flextable object.
      col_keys: `grp`, `n`, `rate`, `percent` 
      header has 1 row(s) 
      body has 11 row(s) 
      original dataset sample: 
          grp   n  rate percent
      1  0-17 100 20000      10
      2 18-24 100 10000      10
      3 25-34 100 10000      10
      4 35-44 100 10000      10
      5 45-54 100 10000      10

# `active_calc_age()` matches snapshot

    Code
      tbl_active
    Output
      # A tibble: 9 x 4
        grp       n percent     rate
        <fct> <int>   <dbl>    <dbl>
      1 0-17    170  0.189  0.000730
      2 18-24    60  0.0667 0.000707
      3 25-34    90  0.1    0.000637
      4 35-44    90  0.1    0.000771
      5 45-54    90  0.1    0.000789
      6 55-64    90  0.1    0.000774
      7 65-74    90  0.1    0.00111 
      8 75-84    90  0.1    0.00252 
      9 85+     130  0.144  0.00889 

