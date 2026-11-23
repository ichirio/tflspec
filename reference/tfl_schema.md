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
those with one –, scales and the limits `xlim()`, `ylim()`, `lims()`,
`expand_limits()`, coordinates, complete themes, `theme()` and its parts
`theme_sub_*()`, elements, guides, positions, `annotation_*()`, a
facet's `vars()` and labellers); any other function of a plot `ggplot2`
exports (`coord_quickmap`, `geom_sf_text`) is its formals. `ggplot2`'s
other spellings (`scale_color_manual`, `guide_colorbar`, `theme_grey`,
`stat_bin2d`) read as the card of the function they spell, and the code
keeps the name written. So are the extension packages' (ggsurvfit's
`add_risktable()` and themes, ggrepel, ggh4x's facets and strips,
ggtext's elements, ggnewscale, patchwork's arrangement): those a
clinical figure uses have a card, any other of a plot is named `pkg::fn`
([`ggh4x::facet_manual`](https://teunbrand.github.io/ggh4x/reference/facet_manual.html)).
The arguments the functions of one kind share – a layer's `stat`,
`position`, `fun.data`, `pad`; a scale's `name`, `breaks`, `labels`,
`sec.axis`; `theme()`'s settings, each its element – are typed and
labelled by `inst/fig/gg_args.csv` for every function that takes them. A
geom given another `stat` (`geom_step(stat = "ecdf")`) is checked with
that stat's parameters too, a stat given another `geom` with that
geom's. A layer takes what any layer passes on to `layer()`
(`key_glyph`, ggplot2 4.0's `layout`) and its position's (ggplot2 4.0's
`nudge_x` / `nudge_y` of `geom_text()`); an argument by ggplot2's other
spelling of an aesthetic (`color`, `pch`, `lty`) is checked as the one
it spells. `theme()` takes ggplot2 4.0's default palettes
(`palette.colour.discrete` ...).

The types, in a short notation: `string`, `integer`, `number` (a number
written as arithmetic of numbers, `!r 1/5`, is one), `logical`,
`enum[a|b]`, `column`, `stat`, `template`, `r` (`!r`), `formula` (`!r`,
written `~ <text>`), `guard` (`!r`, a two-sided `cond ~ "text"`),
`function` (a function by its name, `mean_se` or
`Hmisc::smean.cl.normal`, written bare: `fun.data = mean_se`), `tilde`
(a formula as its text, `y ~ x` or `~ . / 7`, written as the formula),
`date` (a date as its text, `2024-01-01`; a sequence of them, `NA` an
open end, written `as.Date(c("2024-01-01", NA))`), `call` and `call<fn>`
(`{fn:, args:}`; `call<label_*>` takes any function whose name the
pattern matches, of any package: `{fn: scales::label_percent}`; base R's
that clinical figures write as a value are typed where they fit:
[`seq()`](https://rdrr.io/r/base/seq.html) for breaks,
`{fn: seq, args: {from: 0, to: 52, by: 4}}` written
`seq(0, 52, by = 4)`; `alpha()` for a colour; grDevices' palettes,
[`hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html) ..., for a
scale's colours), `list<T>` (a sequence), `vec<K, V>` (a map that is a
named vector in R), `map<K, V>` (a map that is a named list),
`rows<f1, f2, ...>` (a sequence of maps that is a data frame) and `T|U`
(either).

An argument's entry also says how a form shows it: `label`, its short
English name (a GUI translates it and writes "label (argument)"),
`help`, a sentence or two a form's "?" shows (the `description` is what
the AI kit and the reference read), and `main: true` for the arguments a
form shows from the start (the others folded under the function's other
arguments). `colour` is a colour's text (`#RRGGBB`).

A layer's entry (`geom_errorbar`) says its aesthetics too: `aes`, the
names a `mapping` takes (an unknown one warns, as ggplot2 does; `color`
is `colour`), `aes_required`, those it needs, and `aes_or`, a required
one's alternatives (`ymin` or `xmin`: a bar across). A figure's plot is
checked for them: a layer missing a required aesthetic, neither mapped
nor given a value nor inherited from `ggplot()`'s mapping, warns.

A mapping's value is a column's name (`colour: TRTA`), or an expression
of a mapping's form, written as its text, untagged: a call of
[`factor()`](https://rdrr.io/r/base/factor.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html),
[`ordered()`](https://rdrr.io/r/base/factor.html),
[`as.numeric()`](https://rdrr.io/r/base/numeric.html),
[`as.integer()`](https://rdrr.io/r/base/integer.html),
[`as.character()`](https://rdrr.io/r/base/character.html),
[`interaction()`](https://rdrr.io/r/base/interaction.html),
[`reorder()`](https://rdrr.io/r/stats/reorder.factor.html),
[`rev()`](https://rdrr.io/r/base/rev.html),
[`abs()`](https://rdrr.io/r/base/MathFun.html),
[`sqrt()`](https://rdrr.io/r/base/MathFun.html),
[`exp()`](https://rdrr.io/r/base/Log.html),
[`log()`](https://rdrr.io/r/base/Log.html) /
[`log2()`](https://rdrr.io/r/base/Log.html) /
[`log10()`](https://rdrr.io/r/base/Log.html) /
[`log1p()`](https://rdrr.io/r/base/Log.html),
[`round()`](https://rdrr.io/r/base/Round.html), `after_stat()`,
`after_scale()`, `stage()`,
[`ifelse()`](https://rdrr.io/r/base/ifelse.html),
[`cut()`](https://rdrr.io/r/base/cut.html),
[`is.na()`](https://rdrr.io/r/base/NA.html),
[`nchar()`](https://rdrr.io/r/base/nchar.html),
[`toupper()`](https://rdrr.io/r/base/chartr.html) /
[`tolower()`](https://rdrr.io/r/base/chartr.html),
[`substr()`](https://rdrr.io/r/base/substr.html) or
[`trimws()`](https://rdrr.io/r/base/trimws.html), or of the arithmetic
or a comparison (`<`, `<=`, `==`, `!=`, `%in%`, `!`, `&`, `|`), of
columns, constants, their arithmetic and
[`c()`](https://rdrr.io/r/base/c.html),
[`sum()`](https://rdrr.io/r/base/sum.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`mean()`](https://rdrr.io/r/base/mean.html),
[`median()`](https://rdrr.io/r/stats/median.html),
[`length()`](https://rdrr.io/r/base/length.html), `alpha()`,
[`pmin()`](https://rdrr.io/r/base/Extremes.html),
[`pmax()`](https://rdrr.io/r/base/Extremes.html),
[`cumsum()`](https://rdrr.io/r/base/cumsum.html), `[`,
[`duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`unique()`](https://rdrr.io/r/base/unique.html),
[`rank()`](https://rdrr.io/r/base/rank.html) (`x: factor(AVISITN)`,
`y: after_stat(density)`, `x: reorder(USUBJID, -PCHG)`,
`fill: after_scale(alpha(colour, 0.4))`, a waterfall's
`fill: PCHG <= -30` or `fill: ifelse(PCHG <= -30, "PR", "SD/PD")`; in
YAML a text that starts with `!` is quoted, `'!is.na(ADT)'`); a number
is a constant (`group: 1`). A column whose name R does not write bare is
written in backticks (`` x: `A-B` ``, as code is read back); written
tight (`A-B`) it is read as the expression, and the check warns. Any
other R is `!r`. The check warns for a text of neither form (it is
written as a column of that name) and for a variable of `after_stat()`
the layer's stat does not compute (`after_stat(densty)`).

## See also

[`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md)

## Examples

``` r
names(tfl_schema())
#>   [1] "table_plan"                  "plan_sort"                  
#>   [3] "plan_digits"                 "plan_levels"                
#>   [5] "plan_labels"                 "plan_nest"                  
#>   [7] "plan_total"                  "plan_stat_groups"           
#>   [9] "plan_cells"                  "plan_filter"                
#>  [11] "plan_compare"                "plan_stub"                  
#>  [13] "plan_paginate_group"         "plan_row_group"             
#>  [15] "plan_append"                 "plan_blanks"                
#>  [17] "plan_paginate_rows"          "plan_paginate_cols"         
#>  [19] "plan_style"                  "plan_cell_style"            
#>  [21] "plan_hide"                   "plan_empty"                 
#>  [23] "plan_col_header"             "plan_columns"               
#>  [25] "plan_listing"                "listing_col"                
#>  [27] "plan_titles"                 "plan_footnotes"             
#>  [29] "plan_after"                  "rtf_border"                 
#>  [31] "rtf_border_line"             "normalize_ard"              
#>  [33] "rtf_col_header"              "cell_rows"                  
#>  [35] "ard_summary"                 "ard_tabulate"               
#>  [37] "ard_tabulate_value"          "ard_hierarchical"           
#>  [39] "ard_stack_hierarchical"      "continuous_summary_fns"     
#>  [41] "ard_missing"                 "ard_total_n"                
#>  [43] "ard_stack"                   "ard_strata"                 
#>  [45] "ard_categorical_ci"          "ard_stats_t_test"           
#>  [47] "ard_stats_wilcox_test"       "ard_stats_chisq_test"       
#>  [49] "ard_stats_fisher_test"       "ard_stats_kruskal_test"     
#>  [51] "ard_stats_oneway_test"       "rtf_page"                   
#>  [53] "rtf_style"                   "rtf_default_format"         
#>  [55] "rtf_document"                "rtf_watermark"              
#>  [57] "rtf_tables"                  "rtf_figures"                
#>  [59] "rtf_titles"                  "rtf_footnotes"              
#>  [61] "generate_rtfreport"          "rtf_header"                 
#>  [63] "rtf_footer"                  "ggplot"                     
#>  [65] "ggsurvfit"                   "ggcuminc"                   
#>  [67] "tfl_risk_panel"              "tfl_text_column"            
#>  [69] "tfl_n_panel"                 "geom_line"                  
#>  [71] "geom_point"                  "geom_errorbar"              
#>  [73] "geom_pointrange"             "geom_col"                   
#>  [75] "geom_text"                   "geom_label"                 
#>  [77] "geom_step"                   "geom_area"                  
#>  [79] "geom_ribbon"                 "geom_segment"               
#>  [81] "geom_tile"                   "geom_boxplot"               
#>  [83] "geom_violin"                 "geom_histogram"             
#>  [85] "geom_density"                "geom_jitter"                
#>  [87] "geom_smooth"                 "geom_hline"                 
#>  [89] "geom_vline"                  "geom_abline"                
#>  [91] "geom_text_repel"             "geom_sina"                  
#>  [93] "annotate"                    "tfl_ard_number"             
#>  [95] "new_scale_colour"            "add_censor_mark"            
#>  [97] "add_confidence_interval"     "add_risktable"              
#>  [99] "add_quantile"                "add_pvalue"                 
#> [101] "add_legend_title"            "add_risktable_strata_symbol"
#> [103] "scale_x_continuous"          "scale_y_continuous"         
#> [105] "scale_x_log10"               "scale_y_log10"              
#> [107] "scale_x_discrete"            "scale_colour_tfl"           
#> [109] "scale_fill_tfl"              "scale_colour_manual"        
#> [111] "scale_fill_manual"           "scale_shape_manual"         
#> [113] "scale_linetype_manual"       "coord_cartesian"            
#> [115] "coord_equal"                 "coord_flip"                 
#> [117] "facet_wrap"                  "facet_grid"                 
#> [119] "labs"                        "theme_tfl"                  
#> [121] "theme"                       "guides"                     
#> [123] "geom_bar"                    "geom_linerange"             
#> [125] "geom_crossbar"               "geom_rect"                  
#> [127] "geom_path"                   "geom_rug"                   
#> [129] "geom_freqpoly"               "geom_dotplot"               
#> [131] "geom_raster"                 "geom_polygon"               
#> [133] "geom_curve"                  "geom_count"                 
#> [135] "geom_qq"                     "geom_qq_line"               
#> [137] "stat_ecdf"                   "stat_summary"               
#> [139] "stat_summary_bin"            "stat_ellipse"               
#> [141] "stat_unique"                 "geom_blank"                 
#> [143] "geom_spoke"                  "geom_quantile"              
#> [145] "geom_contour"                "geom_contour_filled"        
#> [147] "geom_density_2d"             "geom_density_2d_filled"     
#> [149] "geom_bin_2d"                 "geom_function"              
#> [151] "stat_smooth"                 "stat_bin"                   
#> [153] "stat_count"                  "stat_boxplot"               
#> [155] "stat_density"                "stat_ydensity"              
#> [157] "stat_identity"               "stat_function"              
#> [159] "stat_qq"                     "stat_qq_line"               
#> [161] "stat_sum"                    "position_dodge"             
#> [163] "position_dodge2"             "position_jitter"            
#> [165] "position_jitterdodge"        "position_nudge"             
#> [167] "position_stack"              "position_fill"              
#> [169] "position_identity"           "scale_y_discrete"           
#> [171] "scale_x_reverse"             "scale_y_reverse"            
#> [173] "scale_x_sqrt"                "scale_y_sqrt"               
#> [175] "scale_x_date"                "scale_y_date"               
#> [177] "scale_x_datetime"            "scale_colour_brewer"        
#> [179] "scale_fill_brewer"           "scale_colour_distiller"     
#> [181] "scale_fill_distiller"        "scale_colour_gradient"      
#> [183] "scale_fill_gradient"         "scale_colour_gradient2"     
#> [185] "scale_fill_gradient2"        "scale_colour_gradientn"     
#> [187] "scale_fill_gradientn"        "scale_colour_viridis_d"     
#> [189] "scale_fill_viridis_d"        "scale_colour_viridis_c"     
#> [191] "scale_fill_viridis_c"        "scale_colour_grey"          
#> [193] "scale_fill_grey"             "scale_colour_hue"           
#> [195] "scale_fill_hue"              "scale_colour_identity"      
#> [197] "scale_fill_identity"         "scale_shape"                
#> [199] "scale_linetype"              "scale_size"                 
#> [201] "scale_size_manual"           "scale_size_area"            
#> [203] "scale_alpha"                 "scale_alpha_manual"         
#> [205] "scale_linewidth"             "scale_linewidth_manual"     
#> [207] "scale_colour_discrete"       "scale_fill_discrete"        
#> [209] "scale_colour_continuous"     "scale_fill_continuous"      
#> [211] "scale_x_binned"              "scale_colour_steps"         
#> [213] "scale_fill_steps"            "scale_colour_steps2"        
#> [215] "scale_fill_steps2"           "scale_colour_fermenter"     
#> [217] "scale_fill_fermenter"        "scale_x_time"               
#> [219] "expansion"                   "sec_axis"                   
#> [221] "dup_axis"                    "coord_fixed"                
#> [223] "coord_trans"                 "coord_polar"                
#> [225] "coord_radial"                "xlab"                       
#> [227] "ylab"                        "ggtitle"                    
#> [229] "theme_bw"                    "theme_classic"              
#> [231] "theme_minimal"               "theme_light"                
#> [233] "theme_linedraw"              "theme_gray"                 
#> [235] "theme_void"                  "theme_dark"                 
#> [237] "element_text"                "element_line"               
#> [239] "element_rect"                "element_blank"              
#> [241] "margin"                      "guide_legend"               
#> [243] "guide_colourbar"             "guide_axis"                 
#> [245] "guide_none"                  "guide_bins"                 
#> [247] "guide_coloursteps"           "guide_axis_logticks"        
#> [249] "guide_axis_theta"            "guide_axis_stack"           
#> [251] "geom_hex"                    "stat_bin_2d"                
#> [253] "stat_bin_hex"                "stat_contour"               
#> [255] "stat_contour_filled"         "stat_density_2d"            
#> [257] "stat_density_2d_filled"      "stat_summary_2d"            
#> [259] "stat_summary_hex"            "stat_quantile"              
#> [261] "stat_align"                  "stat_connect"               
#> [263] "stat_manual"                 "annotation_logticks"        
#> [265] "annotation_custom"           "annotation_raster"          
#> [267] "expand_limits"               "xlim"                       
#> [269] "ylim"                        "lims"                       
#> [271] "scale_colour_ordinal"        "scale_fill_ordinal"         
#> [273] "scale_colour_binned"         "scale_fill_binned"          
#> [275] "scale_colour_stepsn"         "scale_fill_stepsn"          
#> [277] "scale_colour_viridis_b"      "scale_fill_viridis_b"       
#> [279] "scale_colour_date"           "scale_fill_date"            
#> [281] "scale_colour_datetime"       "scale_fill_datetime"        
#> [283] "scale_y_binned"              "scale_y_datetime"           
#> [285] "scale_y_time"                "scale_alpha_continuous"     
#> [287] "scale_alpha_discrete"        "scale_alpha_binned"         
#> [289] "scale_alpha_ordinal"         "scale_alpha_identity"       
#> [291] "scale_size_continuous"       "scale_size_discrete"        
#> [293] "scale_size_binned"           "scale_size_ordinal"         
#> [295] "scale_size_identity"         "scale_size_binned_area"     
#> [297] "scale_radius"                "scale_shape_discrete"       
#> [299] "scale_shape_binned"          "scale_shape_ordinal"        
#> [301] "scale_shape_identity"        "scale_linetype_discrete"    
#> [303] "scale_linetype_binned"       "scale_linetype_identity"    
#> [305] "scale_linewidth_continuous"  "scale_linewidth_discrete"   
#> [307] "scale_linewidth_binned"      "scale_linewidth_ordinal"    
#> [309] "scale_linewidth_identity"    "scale_discrete_manual"      
#> [311] "scale_discrete_identity"     "scale_continuous_identity"  
#> [313] "coord_transform"             "facet_null"                 
#> [315] "vars"                        "labeller"                   
#> [317] "label_value"                 "label_both"                 
#> [319] "label_context"               "label_parsed"               
#> [321] "label_wrap_gen"              "label_bquote"               
#> [323] "theme_test"                  "theme_sub_axis"             
#> [325] "theme_sub_axis_x"            "theme_sub_axis_y"           
#> [327] "theme_sub_axis_bottom"       "theme_sub_axis_top"         
#> [329] "theme_sub_axis_left"         "theme_sub_axis_right"       
#> [331] "theme_sub_legend"            "theme_sub_panel"            
#> [333] "theme_sub_plot"              "theme_sub_strip"            
#> [335] "element_geom"                "element_point"              
#> [337] "element_polygon"             "rel"                        
#> [339] "unit"                        "margin_auto"                
#> [341] "margin_part"                 "arrow"                      
#> [343] "guide_custom"                "geom_label_repel"           
#> [345] "geom_richtext"               "new_scale_fill"             
#> [347] "facet_nested"                "facet_nested_wrap"          
#> [349] "facetted_pos_scales"         "element_markdown"           
#> [351] "element_textbox_simple"      "stat_stepribbon"            
#> [353] "scale_ggsurvfit"             "theme_ggsurvfit_default"    
#> [355] "theme_ggsurvfit_KMunicate"   "theme_risktable_default"    
#> [357] "theme_risktable_boxed"       "position_nudge_repel"       
#> [359] "geom_textbox"                "element_textbox"            
#> [361] "strip_nested"                "strip_themed"               
#> [363] "strip_vanilla"               "elem_list_text"             
#> [365] "elem_list_rect"              "facet_grid2"                
#> [367] "facet_wrap2"                 "force_panelsizes"           
#> [369] "new_scale"                   "wrap_plots"                 
#> [371] "plot_layout"                 "plot_annotation"            
#> [373] "plot_spacer"                 "wrap_elements"              
#> [375] "inset_element"               "guide_area"                 
#> [377] "area"                        "free"                       
#> [379] "tfl_image"                   "km_curve"                   
#> [381] "km_ci"                       "censor_mark"                
#> [383] "ref_label"                   "risk_table"                 
#> [385] "ard_number"                  "n_table"                    
#> [387] "text_column"                 "geom"                       
#> [389] "layer_code"                  "call"                       
#> [391] "figure"                      "line"                       
#> [393] "point"                       "errorbar"                   
#> [395] "pointrange"                  "col"                        
#> [397] "text"                        "label"                      
#> [399] "step"                        "ribbon"                     
#> [401] "segment"                     "tile"                       
#> [403] "boxplot"                     "violin"                     
#> [405] "histogram"                   "density"                    
#> [407] "jitter"                      "smooth"                     
#> [409] "hline"                       "vline"                      
#> [411] "abline"                      "text_repel"                 
#> [413] "sina"                        "errorbar_h"                 
#> [415] "beeswarm"                   
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
