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
#>  [55] "rtf_footnotes"           "generate_rtfreport"     
#>  [57] "rtf_header"              "rtf_footer"             
#>  [59] "ggplot"                  "ggsurvfit"              
#>  [61] "ggcuminc"                "tfl_risk_panel"         
#>  [63] "tfl_text_column"         "tfl_n_panel"            
#>  [65] "geom_line"               "geom_point"             
#>  [67] "geom_errorbar"           "geom_pointrange"        
#>  [69] "geom_col"                "geom_text"              
#>  [71] "geom_label"              "geom_step"              
#>  [73] "geom_area"               "geom_ribbon"            
#>  [75] "geom_segment"            "geom_tile"              
#>  [77] "geom_boxplot"            "geom_violin"            
#>  [79] "geom_histogram"          "geom_density"           
#>  [81] "geom_jitter"             "geom_smooth"            
#>  [83] "geom_hline"              "geom_vline"             
#>  [85] "geom_abline"             "geom_text_repel"        
#>  [87] "geom_sina"               "annotate"               
#>  [89] "tfl_ard_number"          "new_scale_colour"       
#>  [91] "add_censor_mark"         "add_confidence_interval"
#>  [93] "add_risktable"           "add_quantile"           
#>  [95] "add_pvalue"              "scale_x_continuous"     
#>  [97] "scale_y_continuous"      "scale_x_log10"          
#>  [99] "scale_y_log10"           "scale_x_discrete"       
#> [101] "scale_colour_tfl"        "scale_fill_tfl"         
#> [103] "scale_colour_manual"     "scale_fill_manual"      
#> [105] "scale_shape_manual"      "scale_linetype_manual"  
#> [107] "coord_cartesian"         "coord_equal"            
#> [109] "coord_flip"              "facet_wrap"             
#> [111] "facet_grid"              "labs"                   
#> [113] "theme_tfl"               "theme"                  
#> [115] "guides"                  "geom_label_repel"       
#> [117] "geom_richtext"           "new_scale_fill"         
#> [119] "facet_nested"            "facet_nested_wrap"      
#> [121] "facetted_pos_scales"     "element_markdown"       
#> [123] "element_textbox_simple"  "wrap_plots"             
#> [125] "plot_layout"             "plot_annotation"        
#> [127] "tfl_image"               "km_curve"               
#> [129] "km_ci"                   "censor_mark"            
#> [131] "ref_label"               "risk_table"             
#> [133] "ard_number"              "n_table"                
#> [135] "text_column"             "geom"                   
#> [137] "layer_code"              "call"                   
#> [139] "figure"                  "line"                   
#> [141] "point"                   "errorbar"               
#> [143] "pointrange"              "col"                    
#> [145] "text"                    "label"                  
#> [147] "step"                    "area"                   
#> [149] "ribbon"                  "segment"                
#> [151] "tile"                    "boxplot"                
#> [153] "violin"                  "histogram"              
#> [155] "density"                 "jitter"                 
#> [157] "smooth"                  "hline"                  
#> [159] "vline"                   "abline"                 
#> [161] "text_repel"              "sina"                   
#> [163] "errorbar_h"              "beeswarm"               
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
