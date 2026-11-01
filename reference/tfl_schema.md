# The spec schema: the arguments a spec item may give, and their types

A spec's `content:` lists items, one a function call
([`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)):
the function, its arguments by name. The schema says what each takes –
the type of a value where the spec writes it differently from R (a YAML
map for a named vector, a list of rows for a data frame, `!r` for an
expression), and the grammar of `...` for the functions whose body is
`...` (`plan_cells()`, `plan_digits()`, `plan_sort()` and ten more). The
same file is what the GUI's help and the AI kit's schema read.

## Usage

``` r
tfl_schema(fn = NULL)
```

## Arguments

- fn:

  A function's name (`"plan_cells"`), or `NULL` for all.

## Value

The function's entry (`package`, `description`, `dots`, `args`; `rank`,
its place in the recommended order, and `stage`, `"mapping"`,
`"finishing"` or `"output"`; `formals`, the function's own arguments but
the data's (`x`, `plan`, `.plan`, `data`, `mapping`), and `required`,
those of them with no default value), or the whole schema, a list by
function; `NULL` for a function the schema does not have.

## Details

A figure's functions: the file has a card for those a clinical figure
uses (the geoms and the stats – those with no geom of their own and
those with one –, scales, coordinates, complete themes and `theme()`,
elements, guides, positions); any other function of a plot `ggplot2`
exports (`scale_shape_identity`, `guide_custom`) is its formals. The
arguments the functions of one kind share – a layer's `stat`,
`position`, `fun.data`, `pad`; a scale's `name`, `breaks`, `labels`,
`sec.axis`; `theme()`'s settings, each its element – are typed and
labelled by `inst/fig/gg_args.csv` for every function that takes them. A
geom given another `stat` (`geom_step(stat = "ecdf")`) is checked with
that stat's parameters too, a stat given another `geom` with that
geom's.

The types, in a short notation: `string`, `integer`, `number`,
`logical`, `enum[a|b]`, `column`, `stat`, `template`, `r` (`!r`),
`formula` (`!r`, written `~ <text>`), `guard` (`!r`, a two-sided
`cond ~ "text"`), `call` and `call<fn>` (`{fn:, args:}`), `list<T>` (a
sequence), `vec<K, V>` (a map that is a named vector in R), `map<K, V>`
(a map that is a named list), `rows<f1, f2, ...>` (a sequence of maps
that is a data frame) and `T|U` (either).

An argument's entry also says how a form shows it: `label`, its short
English name (a GUI translates it and writes "label (argument)"),
`help`, a sentence or two a form's "?" shows (the `description` is what
the AI kit and the reference read), and `main: true` for the arguments a
form shows from the start (the others folded under the function's other
arguments). `colour` is a colour's text (`#RRGGBB`).

## See also

[`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md)

## Examples

``` r
names(tfl_schema())
#>   [1] "table_plan"              "plan_sort"              
#>   [3] "plan_digits"             "plan_levels"            
#>   [5] "plan_labels"             "plan_nest"              
#>   [7] "plan_total"              "plan_stat_groups"       
#>   [9] "plan_cells"              "plan_filter"            
#>  [11] "plan_compare"            "plan_stub"              
#>  [13] "plan_paginate_group"     "plan_row_group"         
#>  [15] "plan_append"             "plan_blanks"            
#>  [17] "plan_paginate_rows"      "plan_paginate_cols"     
#>  [19] "plan_style"              "plan_cell_style"        
#>  [21] "plan_hide"               "plan_empty"             
#>  [23] "plan_col_header"         "plan_columns"           
#>  [25] "plan_listing"            "listing_col"            
#>  [27] "plan_titles"             "plan_footnotes"         
#>  [29] "plan_after"              "rtf_border"             
#>  [31] "normalize_ard"           "rtf_col_header"         
#>  [33] "cell_rows"               "ard_summary"            
#>  [35] "ard_tabulate"            "ard_tabulate_value"     
#>  [37] "ard_hierarchical"        "ard_stack_hierarchical" 
#>  [39] "continuous_summary_fns"  "ard_missing"            
#>  [41] "ard_total_n"             "ard_stack"              
#>  [43] "ard_strata"              "ard_categorical_ci"     
#>  [45] "ard_stats_t_test"        "ard_stats_wilcox_test"  
#>  [47] "ard_stats_chisq_test"    "ard_stats_fisher_test"  
#>  [49] "ard_stats_kruskal_test"  "ard_stats_oneway_test"  
#>  [51] "rtf_page"                "rtf_default_format"     
#>  [53] "rtf_document"            "rtf_watermark"          
#>  [55] "rtf_tables"              "rtf_figures"            
#>  [57] "rtf_titles"              "rtf_footnotes"          
#>  [59] "generate_rtfreport"      "rtf_header"             
#>  [61] "rtf_footer"              "ggplot"                 
#>  [63] "ggsurvfit"               "ggcuminc"               
#>  [65] "tfl_risk_panel"          "tfl_text_column"        
#>  [67] "tfl_n_panel"             "geom_line"              
#>  [69] "geom_point"              "geom_errorbar"          
#>  [71] "geom_pointrange"         "geom_col"               
#>  [73] "geom_text"               "geom_label"             
#>  [75] "geom_step"               "geom_area"              
#>  [77] "geom_ribbon"             "geom_segment"           
#>  [79] "geom_tile"               "geom_boxplot"           
#>  [81] "geom_violin"             "geom_histogram"         
#>  [83] "geom_density"            "geom_jitter"            
#>  [85] "geom_smooth"             "geom_hline"             
#>  [87] "geom_vline"              "geom_abline"            
#>  [89] "geom_text_repel"         "geom_sina"              
#>  [91] "annotate"                "tfl_ard_number"         
#>  [93] "new_scale_colour"        "add_censor_mark"        
#>  [95] "add_confidence_interval" "add_risktable"          
#>  [97] "add_quantile"            "add_pvalue"             
#>  [99] "scale_x_continuous"      "scale_y_continuous"     
#> [101] "scale_x_log10"           "scale_y_log10"          
#> [103] "scale_x_discrete"        "scale_colour_tfl"       
#> [105] "scale_fill_tfl"          "scale_colour_manual"    
#> [107] "scale_fill_manual"       "scale_shape_manual"     
#> [109] "scale_linetype_manual"   "coord_cartesian"        
#> [111] "coord_equal"             "coord_flip"             
#> [113] "facet_wrap"              "facet_grid"             
#> [115] "labs"                    "theme_tfl"              
#> [117] "theme"                   "guides"                 
#> [119] "geom_bar"                "geom_linerange"         
#> [121] "geom_crossbar"           "geom_rect"              
#> [123] "geom_path"               "geom_rug"               
#> [125] "geom_freqpoly"           "geom_dotplot"           
#> [127] "geom_raster"             "geom_polygon"           
#> [129] "geom_curve"              "geom_count"             
#> [131] "geom_qq"                 "geom_qq_line"           
#> [133] "stat_ecdf"               "stat_summary"           
#> [135] "stat_summary_bin"        "stat_ellipse"           
#> [137] "stat_unique"             "geom_blank"             
#> [139] "geom_spoke"              "geom_quantile"          
#> [141] "geom_contour"            "geom_contour_filled"    
#> [143] "geom_density_2d"         "geom_density_2d_filled" 
#> [145] "geom_bin_2d"             "geom_function"          
#> [147] "stat_smooth"             "stat_bin"               
#> [149] "stat_count"              "stat_boxplot"           
#> [151] "stat_density"            "stat_ydensity"          
#> [153] "stat_identity"           "stat_function"          
#> [155] "stat_qq"                 "stat_qq_line"           
#> [157] "stat_sum"                "position_dodge"         
#> [159] "position_dodge2"         "position_jitter"        
#> [161] "position_jitterdodge"    "position_nudge"         
#> [163] "position_stack"          "position_fill"          
#> [165] "position_identity"       "scale_y_discrete"       
#> [167] "scale_x_reverse"         "scale_y_reverse"        
#> [169] "scale_x_sqrt"            "scale_y_sqrt"           
#> [171] "scale_x_date"            "scale_y_date"           
#> [173] "scale_x_datetime"        "scale_colour_brewer"    
#> [175] "scale_fill_brewer"       "scale_colour_distiller" 
#> [177] "scale_fill_distiller"    "scale_colour_gradient"  
#> [179] "scale_fill_gradient"     "scale_colour_gradient2" 
#> [181] "scale_fill_gradient2"    "scale_colour_gradientn" 
#> [183] "scale_fill_gradientn"    "scale_colour_viridis_d" 
#> [185] "scale_fill_viridis_d"    "scale_colour_viridis_c" 
#> [187] "scale_fill_viridis_c"    "scale_colour_grey"      
#> [189] "scale_fill_grey"         "scale_colour_hue"       
#> [191] "scale_fill_hue"          "scale_colour_identity"  
#> [193] "scale_fill_identity"     "scale_shape"            
#> [195] "scale_linetype"          "scale_size"             
#> [197] "scale_size_manual"       "scale_size_area"        
#> [199] "scale_alpha"             "scale_alpha_manual"     
#> [201] "scale_linewidth"         "scale_linewidth_manual" 
#> [203] "scale_colour_discrete"   "scale_fill_discrete"    
#> [205] "scale_colour_continuous" "scale_fill_continuous"  
#> [207] "scale_x_binned"          "scale_colour_steps"     
#> [209] "scale_fill_steps"        "scale_colour_steps2"    
#> [211] "scale_fill_steps2"       "scale_colour_fermenter" 
#> [213] "scale_fill_fermenter"    "scale_x_time"           
#> [215] "expansion"               "sec_axis"               
#> [217] "dup_axis"                "coord_fixed"            
#> [219] "coord_trans"             "coord_polar"            
#> [221] "coord_radial"            "xlab"                   
#> [223] "ylab"                    "ggtitle"                
#> [225] "theme_bw"                "theme_classic"          
#> [227] "theme_minimal"           "theme_light"            
#> [229] "theme_linedraw"          "theme_gray"             
#> [231] "theme_void"              "theme_dark"             
#> [233] "element_text"            "element_line"           
#> [235] "element_rect"            "element_blank"          
#> [237] "margin"                  "guide_legend"           
#> [239] "guide_colourbar"         "guide_axis"             
#> [241] "guide_none"              "guide_bins"             
#> [243] "guide_coloursteps"       "guide_axis_logticks"    
#> [245] "guide_axis_theta"        "guide_axis_stack"       
#> [247] "geom_label_repel"        "geom_richtext"          
#> [249] "new_scale_fill"          "facet_nested"           
#> [251] "facet_nested_wrap"       "facetted_pos_scales"    
#> [253] "element_markdown"        "element_textbox_simple" 
#> [255] "wrap_plots"              "plot_layout"            
#> [257] "plot_annotation"         "tfl_image"              
#> [259] "km_curve"                "km_ci"                  
#> [261] "censor_mark"             "ref_label"              
#> [263] "risk_table"              "ard_number"             
#> [265] "n_table"                 "text_column"            
#> [267] "geom"                    "layer_code"             
#> [269] "call"                    "figure"                 
#> [271] "line"                    "point"                  
#> [273] "errorbar"                "pointrange"             
#> [275] "col"                     "text"                   
#> [277] "label"                   "step"                   
#> [279] "area"                    "ribbon"                 
#> [281] "segment"                 "tile"                   
#> [283] "boxplot"                 "violin"                 
#> [285] "histogram"               "density"                
#> [287] "jitter"                  "smooth"                 
#> [289] "hline"                   "vline"                  
#> [291] "abline"                  "text_repel"             
#> [293] "sina"                    "errorbar_h"             
#> [295] "beeswarm"               
tfl_schema("plan_sort")$dots
#> $style
#> [1] "positional"
#> 
#> $key
#> [1] "keys"
#> 
#> $type
#> [1] "list<string>|logical"
#> 
#> $description
#> [1] "The keys to sort by, most important first; a minus for descending; false for the data's order."
#> 
#> $example
#> [1] ".overall" "-n@SOC"   "SOC"      ".depth"   "-n"       "PT"      
#> 
```
