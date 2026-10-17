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

The types, in a short notation: `string`, `integer`, `number`,
`logical`, `enum[a|b]`, `column`, `stat`, `template`, `r` (`!r`),
`formula` (`!r`, written `~ <text>`), `guard` (`!r`, a two-sided
`cond ~ "text"`), `call` and `call<fn>` (`{fn:, args:}`), `list<T>` (a
sequence), `vec<K, V>` (a map that is a named vector in R), `map<K, V>`
(a map that is a named list), `rows<f1, f2, ...>` (a sequence of maps
that is a data frame) and `T|U` (either).

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
#>  [15] "plan_blanks"             "plan_paginate_rows"     
#>  [17] "plan_paginate_cols"      "plan_style"             
#>  [19] "plan_cell_style"         "plan_hide"              
#>  [21] "plan_col_header"         "plan_columns"           
#>  [23] "plan_listing"            "listing_col"            
#>  [25] "plan_titles"             "plan_footnotes"         
#>  [27] "plan_after"              "rtf_border"             
#>  [29] "normalize_ard"           "rtf_col_header"         
#>  [31] "cell_rows"               "ard_summary"            
#>  [33] "ard_tabulate"            "ard_tabulate_value"     
#>  [35] "ard_hierarchical"        "ard_stack_hierarchical" 
#>  [37] "continuous_summary_fns"  "ard_missing"            
#>  [39] "ard_total_n"             "ard_stack"              
#>  [41] "ard_strata"              "ard_categorical_ci"     
#>  [43] "ard_stats_t_test"        "ard_stats_wilcox_test"  
#>  [45] "ard_stats_chisq_test"    "ard_stats_fisher_test"  
#>  [47] "ard_stats_kruskal_test"  "ard_stats_oneway_test"  
#>  [49] "rtf_page"                "rtf_default_format"     
#>  [51] "rtf_document"            "rtf_tables"             
#>  [53] "rtf_figures"             "rtf_titles"             
#>  [55] "rtf_footnotes"           "ggplot"                 
#>  [57] "ggsurvfit"               "ggcuminc"               
#>  [59] "tfl_risk_panel"          "tfl_text_column"        
#>  [61] "tfl_n_panel"             "geom_line"              
#>  [63] "geom_point"              "geom_errorbar"          
#>  [65] "geom_pointrange"         "geom_col"               
#>  [67] "geom_text"               "geom_label"             
#>  [69] "geom_step"               "geom_area"              
#>  [71] "geom_ribbon"             "geom_segment"           
#>  [73] "geom_tile"               "geom_boxplot"           
#>  [75] "geom_violin"             "geom_histogram"         
#>  [77] "geom_density"            "geom_jitter"            
#>  [79] "geom_smooth"             "geom_hline"             
#>  [81] "geom_vline"              "geom_abline"            
#>  [83] "geom_text_repel"         "geom_sina"              
#>  [85] "annotate"                "tfl_ard_number"         
#>  [87] "new_scale_colour"        "add_censor_mark"        
#>  [89] "add_confidence_interval" "add_risktable"          
#>  [91] "add_quantile"            "add_pvalue"             
#>  [93] "scale_x_continuous"      "scale_y_continuous"     
#>  [95] "scale_x_log10"           "scale_y_log10"          
#>  [97] "scale_x_discrete"        "scale_colour_tfl"       
#>  [99] "scale_fill_tfl"          "scale_colour_manual"    
#> [101] "scale_fill_manual"       "scale_shape_manual"     
#> [103] "scale_linetype_manual"   "coord_cartesian"        
#> [105] "coord_equal"             "coord_flip"             
#> [107] "facet_wrap"              "facet_grid"             
#> [109] "labs"                    "theme_tfl"              
#> [111] "theme"                   "guides"                 
#> [113] "geom_label_repel"        "geom_richtext"          
#> [115] "new_scale_fill"          "facet_nested"           
#> [117] "facet_nested_wrap"       "facetted_pos_scales"    
#> [119] "element_markdown"        "element_textbox_simple" 
#> [121] "wrap_plots"              "plot_layout"            
#> [123] "plot_annotation"         "tfl_image"              
#> [125] "km_curve"                "km_ci"                  
#> [127] "censor_mark"             "ref_label"              
#> [129] "risk_table"              "ard_number"             
#> [131] "n_table"                 "text_column"            
#> [133] "geom"                    "layer_code"             
#> [135] "call"                    "figure"                 
#> [137] "line"                    "point"                  
#> [139] "errorbar"                "pointrange"             
#> [141] "col"                     "text"                   
#> [143] "label"                   "step"                   
#> [145] "area"                    "ribbon"                 
#> [147] "segment"                 "tile"                   
#> [149] "boxplot"                 "violin"                 
#> [151] "histogram"               "density"                
#> [153] "jitter"                  "smooth"                 
#> [155] "hline"                   "vline"                  
#> [157] "abline"                  "text_repel"             
#> [159] "sina"                    "errorbar_h"             
#> [161] "beeswarm"               
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
