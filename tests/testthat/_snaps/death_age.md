# `void(death_table_age())` matches snapshot

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

# `death_calc_age()` matches snapshot

    Code
      tbl_death
    Output
      # A tibble: 10 x 4
         grp         n percent     rate
         <fct>   <int>   <dbl>    <dbl>
       1 0-17     1010  0.101   0.00434
       2 18-24    1000  0.100   0.0118 
       3 25-34    1000  0.100   0.00708
       4 35-44    1000  0.100   0.00856
       5 45-54    1000  0.100   0.00877
       6 55-64    1000  0.100   0.00860
       7 65-74    1000  0.100   0.0123 
       8 75-84    1000  0.100   0.0280 
       9 85+      1000  0.100   0.0684 
      10 Missing   989  0.0989 NA      

