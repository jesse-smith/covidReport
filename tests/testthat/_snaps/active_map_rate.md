# `active_map_rate()` matches snapshot

    Code
      plt[["data"]]
    Output
      # A tibble: 30 x 6
         zip                   n pop_2019                    geometry  rate zip_rt_lbl
         <chr>             <int>    <int> <MULTIPOLYGON [US_survey_f> <dbl> <chr>     
       1 38016                25    48325 (((860282.8 334206.6, 8603~  51.7 "38016\n5~
       2 38017                25    55073 (((881714.7 331007.1, 8817~  45.4 "38017\n4~
       3 38018+38028          50    42944 (((881770.2 353083.4, 8818~ 116.  "38018+38~
       4 38053                25    26282 (((865635.1 396086.9, 8677~  95.1 "38053\n9~
       5 38103+38104+38105    50    44055 (((759942 335694.8, 759983~ 113.  "38103+38~
       6 38106+38126          50    30120 (((758217.9 318401.2, 7590~ 166.  "38106+38~
       7 38107+38108          50    33410 (((762067.9 336711.2, 7624~ 150.  "38107+38~
       8 38109                25    44911 (((756515.4 293812.6, 7565~  55.7 "38109\n5~
       9 38111                25    42846 (((794333.7 318151.8, 7943~  58.3 "38111\n5~
      10 38112+38122          50    40931 (((795974.7 327671.4, 7962~ 122.  "38112+38~
      # ... with 20 more rows

---

    Code
      plt[["layers"]]
    Output
      [[1]]
      mapping:  
      geom_sf: na.rm = FALSE
      stat_sf: na.rm = FALSE
      position_identity 
      
      [[2]]
      mapping: label = ~.data[["zip"]] 
      geom_text: parse = FALSE, check_overlap = FALSE, na.rm = FALSE
      stat_sf_coordinates: na.rm = FALSE, fun.geometry = NULL
      position_identity 
      
      [[3]]
      mapping: x = ~x, y = ~y 
      geom_label: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      

---

    Code
      plt[["scales"]]
    Output
      <ggproto object: Class ScalesList, gg>
          add: function
          clone: function
          find: function
          get_scales: function
          has_scale: function
          input: function
          n: function
          non_position_scales: function
          scales: list
          super:  <ggproto object: Class ScalesList, gg>

---

    Code
      plt[["mapping"]]
    Output
      Aesthetic mapping: 
      * `geometry` -> `.data[["geometry"]]`
      * `fill`     -> `.data[["rate"]]`

---

    Code
      plt[["theme"]]
    Output
      List of 93
       $ line                      :List of 6
        ..$ colour       : chr "black"
        ..$ size         : num 0.636
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                      :List of 5
        ..$ fill         : Named chr "#F0F0F0"
        .. ..- attr(*, "names")= chr "Light Gray"
        ..$ colour       : logi NA
        ..$ size         : num 0.636
        ..$ linetype     : num 0
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                      :List of 11
        ..$ family       : chr "sans"
        ..$ face         : chr "plain"
        ..$ colour       : Named chr "#3C3C3C"
        .. ..- attr(*, "names")= chr "Dark Gray"
        ..$ size         : int 14
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                     : NULL
       $ aspect.ratio              : NULL
       $ axis.title                : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.title.x              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 3.5points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top          :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 3.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom       : NULL
       $ axis.title.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 3.5points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left         : NULL
       $ axis.title.y.right        :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 3.5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                 : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.text.x               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.8points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top           :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.8points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom        : NULL
       $ axis.text.y               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.8points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left          : NULL
       $ axis.text.y.right         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.8points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.ticks.x              : NULL
       $ axis.ticks.x.top          : NULL
       $ axis.ticks.x.bottom       : NULL
       $ axis.ticks.y              : NULL
       $ axis.ticks.y.left         : NULL
       $ axis.ticks.y.right        : NULL
       $ axis.ticks.length         : 'simpleUnit' num 3.5points
        ..- attr(*, "unit")= int 8
       $ axis.ticks.length.x       : NULL
       $ axis.ticks.length.x.top   : NULL
       $ axis.ticks.length.x.bottom: NULL
       $ axis.ticks.length.y       : NULL
       $ axis.ticks.length.y.left  : NULL
       $ axis.ticks.length.y.right : NULL
       $ axis.line                 : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.x               : NULL
       $ axis.line.x.top           : NULL
       $ axis.line.x.bottom        : NULL
       $ axis.line.y               : NULL
       $ axis.line.y.left          : NULL
       $ axis.line.y.right         : NULL
       $ legend.background         :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin             : 'margin' num [1:4] 7points 7points 7points 7points
        ..- attr(*, "unit")= int 8
       $ legend.spacing            : 'simpleUnit' num 14points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x          : NULL
       $ legend.spacing.y          : NULL
       $ legend.key                :List of 5
        ..$ fill         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.key.size           : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height         : NULL
       $ legend.key.width          : NULL
       $ legend.text               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.align         : NULL
       $ legend.title              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.title.align        : NULL
       $ legend.position           : num [1:2] 0.11 0.5
       $ legend.direction          : chr "vertical"
       $ legend.justification      : chr "center"
       $ legend.box                : chr "vertical"
       $ legend.box.just           : NULL
       $ legend.box.margin         : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background     : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing        : 'simpleUnit' num 14points
        ..- attr(*, "unit")= int 8
       $ panel.background          :List of 5
        ..$ fill         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.border              :List of 5
        ..$ fill         : logi NA
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.spacing             : 'simpleUnit' num 7points
        ..- attr(*, "unit")= int 8
       $ panel.spacing.x           : NULL
       $ panel.spacing.y           : NULL
       $ panel.grid                :List of 6
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.minor          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.major.x        : NULL
       $ panel.grid.major.y        : NULL
       $ panel.grid.minor.x        : NULL
       $ panel.grid.minor.y        : NULL
       $ panel.ontop               : logi FALSE
       $ plot.background           :List of 5
        ..$ fill         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ plot.title                :List of 11
        ..$ family       : NULL
        ..$ face         : chr "bold"
        ..$ colour       : NULL
        ..$ size         : num 18
        ..$ hjust        : num 0.5
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 7points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.title.position       : chr "panel"
       $ plot.subtitle             :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 16
        ..$ hjust        : num 0.5
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 7points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption              :List of 11
        ..$ family       : NULL
        ..$ face         : chr "italic"
        ..$ colour       : NULL
        ..$ size         : num 12
        ..$ hjust        : num 0.5
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 7points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption.position     : chr "panel"
       $ plot.tag                  :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.tag.position         : chr "topleft"
       $ plot.margin               : 'simpleUnit' num [1:4] 1lines 1lines 1lines 1lines
        ..- attr(*, "unit")= int 3
       $ strip.background          :List of 5
        ..$ fill         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ strip.background.x        : NULL
       $ strip.background.y        : NULL
       $ strip.placement           : chr "inside"
       $ strip.text                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5.6points 5.6points 5.6points 5.6points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x              : NULL
       $ strip.text.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.switch.pad.grid     : 'simpleUnit' num 3.5points
        ..- attr(*, "unit")= int 8
       $ strip.switch.pad.wrap     : 'simpleUnit' num 3.5points
        ..- attr(*, "unit")= int 8
       $ strip.text.y.left         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE

---

    Code
      plt[["coordinates"]]
    Output
      <ggproto object: Class CoordSf, CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          crs: NULL
          datum: crs
          default: TRUE
          default_crs: NULL
          determine_crs: function
          distance: function
          expand: TRUE
          fixup_graticule_labels: function
          get_default_crs: function
          is_free: function
          is_linear: function
          label_axes: list
          label_graticule: 
          labels: function
          limits: list
          lims_method: cross
          modify_scales: function
          ndiscr: 100
          params: list
          range: function
          record_bbox: function
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordSf, CoordCartesian, Coord, gg>

---

    Code
      plt[["facet"]]
    Output
      <ggproto object: Class FacetNull, Facet, gg>
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panels: function
          finish_data: function
          init_scales: function
          map_data: function
          params: list
          setup_data: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetNull, Facet, gg>

---

    Code
      plt[["labels"]]
    Output
      $title
      [1] "Active Cases by ZIP Code"
      
      $subtitle
      [1] "04/01/2021"
      
      $caption
      [1] "Data Source: National Electronic Disease Surveillance System (NEDSS)\nTotal = 900 Mapped = 725 (Other/Missing ZIP = 175)\nRates calculated with ACS 2019 5 Year population estimates"
      
      $geometry
      [1] "geometry"
      
      $fill
      [1] "rate"
      
      $label
      [1] "zip"
      
      $x
      [1] "x"
      
      $y
      [1] "y"
      

