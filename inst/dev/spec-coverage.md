# SPEC coverage

How much of each package the SPEC (a report's `content:` items, checked by
`tfl_schema_check()`, catalogued by `inst/spec/schema.yml`) can write.

## cards / cardx (table and listing SPEC: the ARD definition)

How much of cards' and cardx's ARD functions an analysis row (the ARD
definition's `analyses`: `method`, `by`, `variables`, `strata`,
`denominator`, `statistics`, `args`, `post`) can write.  One row an
argument: `spec-coverage-cards.csv` (columns `package`, `function`,
`argument`, `covered`, `where`, `note`, `group`, `weight`), made by
`dev/spec-coverage-cards.R` from the installed cards / cardx (their
data.frame methods' formals, as `tfl_ard_args()` reads them) and the
catalog (`inst/ard/functions.csv`, `args.csv`).  Scope: every exported
`ard_*()`, and cards' helpers a row writes in `post` (`add_calculated_row()`,
`filter_ard_hierarchical()`, `sort_ard_hierarchical()`,
`diff_ard_hierarchical()`, `replace_null_statistic()`,
`update_ard_stat_label()`).

* **yes**: the row's own column writes it, or the catalog describes it
  (its kind and a hint: the GUI shows it) and the check judges the value
  written in `args` / `post` by that kind (TRUE / FALSE, a number, one of
  the choices, text, a formula, a column, a list, selectors, an expression).
* **partial**: written as R in `args` / `post` and its name checked against
  the installed function, but not described, or its value taken as it is.
* **no**: a SPEC cannot write it.
* `[by design]`: not offered for a new analysis (the old names, the survey
  designs, `ard_formals()`; see `?tfl_ard_functions`) -- left out.

A function's coverage is its arguments' mean (yes 1, partial 0.5); the
percentage is the functions' mean weighted by their use in clinical tables:
3 for what demographics, disposition, AE by SOC / PT and severity, lab
summary and shift, vital signs and model-based efficacy (ANCOVA / LS means,
CMH, KM, log-rank, Cox, incidence rates) use; 2 sometimes; 1 rarely.

cards 0.9.0, cardx 0.3.4; 60 functions, 308 arguments.

| Scope | 2026-10-29 before | 2026-10-29 after |
|---|---:|---:|
| **cards / cardx, weighted** | **75.9%** (153 yes / 155 partial) | **99.3%** (304 / 4) |
| cards ard_*() | 79.9% | 99.4% |
| cardx ard_*() | 76.0% | 99.8% |
| cards post helpers | 63.9% | 96.1% |
| weight-3 functions only | 75.1% | 99.6% |

### Added in this run

* The check judges the values `args` writes, by each argument's kind in the
  catalog (`.ard_arg_value_problems()`, a warning of `tfl_ard_spec()` like
  an unknown argument): `overall = "yes"`, `conf.level = 95`,
  `method = "wilson_cc"` (not one of `ard_categorical_ci()`'s choices),
  `unit_label = 100`, `formula = 1`, `id = 1`, `method.args = "x"`,
  `value = "Y"` (a list or formula of selectors), `tidy_fun = "tidy"`,
  `x = "fit"`, `stats_to_remove = term`.  Only cards' and cardx's functions;
  a name or a call is the program's to judge.
* A `post` step's arguments are checked against its function
  (`filter_ard_hierarchical(filtr = )`), and their values as above
  (`filter = "n > 1"` in quotes, `sort = "frequency"`).
* The catalog describes the post helpers' arguments (18 rows of
  `inst/ard/args.csv`: `expr`, `stat_name`, `filter`, `var`, `keep_empty`,
  `sort` with its choices, `by_level`, `levels`, `rows`, `stat_names` ...).
* Eight method keywords for the model-based and safety analyses, each its
  cardx function by a name a person reads (and the ARS's method name):
  `cmh` (ard_stats_mantelhaen_test), `prop_diff` (ard_stats_prop_test),
  `lsmeans` / `lsmeans_diff` (ard_emmeans_emmeans / _contrast), `km`
  (ard_survival_survfit), `logrank` (ard_survival_survdiff), `abnormal`
  (ard_tabulate_abnormal), `incidence_rate` (ard_incidence_rate).

### Where the gaps are

The scope is close to its ceiling; what is left and what is next:

1. Taken as R (any value is right for some use): `ard_categorical_ci(weights = )`,
   `ard_pairwise(include = )`, `add_calculated_row(fmt_fun = )` (a number of
   decimals or a function), `replace_null_statistic(value = )`.
2. A `custom` row's code is run as it is: nothing in it is judged.
3. Next: the model functions' own arguments (`ard_regression()` passes
   `...` to the model; `ard_car_anova(type = )`), the denominators' shapes
   in `args` (`denominator = data.frame(...)`), and a wider scope (the
   helpers the generated code writes: `continuous_summary_fns()`,
   `label_round()`).

## rtfreporter (table SPEC)

How much of rtfreporter's exported API (rtfreporter@next) a tflspec SPEC
(tflspec@next: the YAML spec's `content:` and `page:`, the sheets' columns
and `content_more` / `page_more`) can write.  The table, one row per
exported function and argument, is `spec-coverage-rtfreporter.csv`
(columns `package`, `function`, `argument`, `covered`, `where`, `note`);
it is made by `dev/spec_coverage.R` from the installed packages' formals
and `inst/spec/schema.yml`.

* **yes**: a SPEC writes it -- a content item the schema checks, a `page:`
  argument the report code passes on (`R/spec_report.R`), or a value
  tflspec fills itself (the pipe, the page's lines, the study's file).
* **partial**: it can be written, but not as the function's own argument
  (`rtf_tables(cell_valign = )` is `plan_style(cell_valign = )`), or a
  content item the schema has no entry for (no check, rank or help).
* **no**: a SPEC cannot write it.
* Notes `[helper]`: options, inspection helpers, code writers, file
  assemblers, the validation kit and deprecated arguments -- no SPEC item
  by design.  `[out of scope]`: `plan_layers()`, designed in another session.

Percentage: yes counts 1, partial 0.5.

| Run        | All arguments | SPEC-relevant (helpers out) | Functions yes / partial / no |
|------------|-------------------|----------------------------------|------------------------------|
| 2026-10-28 before | 41.9% (287 / 30 / 403 of 720) | 47.5% (287 / 30 / 319) | 38 / 7 / 90 |
| 2026-10-28 after  | 43.3% (300 / 24 / 396 of 720) | 49.1% (300 / 24 / 312) | 40 / 6 / 89 |
| 2026-10-29 before | 43.7% (305 / 25 / 397 of 727) | 49.6% (305 / 25 / 310 of 640) | 41 / 6 / 90 |
| 2026-10-29 after  | 43.8% (306 / 25 / 396 of 727) | 49.8% (306 / 25 / 309 of 640) | 41 / 6 / 90 |

(2026-10-29 before is rtfreporter@next with 7 more arguments --
`plan_append()`, `table_plan(value = )`, `rtfreporter_validation_kit()` --
and the measure's corrections: `rtf_tables(style = )` is said by
`plan_style()`'s `rtf_table_style()` fields (partial), `read_gt` is
deprecated, the validation kit a helper.)

### Added in this run

* `rtf_tables(read_attributes = )`: `page.rtf_tables.read_attributes` (a
  logical in the schema), passed on for a report of one content and of
  several.

### Where the gaps are

Most "no" rows are rtfreporter's lower-level table API -- `rtftable()`,
`widen_ard()`, `as_rtftable(s)()`, the `style_*()`, `set_*()`, `fmt_*()`,
`paginate*()` and `listing_*()` helpers -- which the plan verbs already
stand for: a SPEC says the plan, not these calls.  The gaps that a SPEC
could usefully close next:

1. The border constructors as values (`rtf_border_tfl()`, `rtf_border_box()`,
   `rtf_border_top()` ..., `rtf_table_border()`): a border argument takes
   only `{fn: rtf_border}` in the schema.
2. `rtf_tables(col_spec = )` (per-column align / bold ...): only
   `plan_columns(decimal =, row_title =)` say a part of it.
3. `rtf_section(page =, secinfo =)`, `rtf_document(color_table =)`,
   `generate_rtfreport(program =, report =)`, `rtfplot(width_twips =,
   height_twips =)`.

## ggplot2 (figure SPEC)

Measured by `dev/spec-coverage-ggplot2.R` (one row an argument:
`inst/dev/spec-coverage-ggplot2.csv`) with ggplot2 4.0.3.

- **yes**: the function has a card in the catalog and the argument is typed
  and labelled there or by its kind (`inst/fig/gg_args.csv`) -- the GUI shows
  it, the check types its value.
- **partial**: written and accepted by the check, but untyped, or the
  function has no card (written from its formals).
- **no**: the check does not know it (an error, or "no argument").

A function's coverage is its arguments' mean (yes 1, partial 0.5, no 0);
the percentage is the functions' mean weighted by their use in clinical
figures: 3 for what KM, forest, waterfall, spider / swimmer, box / violin,
line over time with mean +/- SE, bar, scatter, heat map and CDF plots use;
2 sometimes; 1 rarely.  Deprecated and internal arguments (`size` of line
geoms, `fun.y`, `trans`, `label.size` ...) are left out.

Two rounds.  The first counted 2015 arguments (156 functions and geoms with
a stat); the second widened the scope by 19 functions (line widths, binned / stepped / default
colour scales, scale_x_time(), guide_axis_logticks() for a forest plot's
ratio axis, guide_axis_theta() / guide_axis_stack(), filled contours) to
2303 arguments of 175 functions.

| Scope | Start | Round 1 | Round 2 (wider scope: before / after) |
|---|---:|---:|---:|
| **ggplot2: geoms (with their stats), scales, coords, themes, facets, guides, labs, positions** | **24.7%** | **94.9%** | **91.4% / 100.0%** |
| geom_*() | 55.9% | 91.4% | 90.6% / 100.0% |
| a geom with another stat (geom_step(stat = "ecdf"), geom_pointrange(stat = "summary") ...) | 0.0% | 100.0% | 100.0% / 100.0% |
| scale_*() and expansion() / sec_axis() / dup_axis() | 8.1% | 95.9% | 87.7% / 100.0% |
| coord_*() | 34.5% | 98.8% | 98.8% / 100.0% |
| theme(), complete themes, element_*(), margin() | 5.0% | 91.2% | 91.2% / 100.0% |
| facet_wrap() / facet_grid() | 57.4% | 100.0% | 100.0% / 100.0% |
| guides() and guide_*() | 10.7% | 97.3% | 86.8% / 100.0% |
| labs(), xlab(), ylab(), ggtitle(), annotate() | 38.8% | 97.4% | 97.4% / 100.0% |
| position_*() | 0.0% | 100.0% | 100.0% / 100.0% |
| **stat_*()** (shown apart) | **44.2%** | **73.1%** | **73.1% / 100.0%** |

Round 2, 2303 arguments: before 1533 yes / 770 partial / 0 no; after 2303
yes.  (ggplot2 4.0's `box_gp`, `whisker_gp` ... are left out: geom_boxplot()
makes them from `whisker.colour` and the like, and given too they clash.)

100% is this measure's ceiling, not ggplot2's: every argument in the scope
has a card and a type, which says the GUI shows it and the check types its
value; the tests run the code of a sample of them (test-fig-ggplot2.R).
The extension packages are measured apart (below).  Next: the scope's
edge (geom_sf, geom_hex, the rarer stat_*()).

### The extension packages (ggsurvfit, ggrepel, ggh4x, ggtext, ggnewscale, patchwork)

The same measure, the same file (`package` says which), over the
functions of a plot of the extension packages tflspec suggests: 46
functions, 351 arguments (ggsurvfit 1.2.1, ggrepel 0.9.8, ggh4x 0.3.1,
ggtext 0.2.0, ggnewscale 0.5.2, patchwork 1.3.2).  Weights: 3 for a KM
plot's (ggsurvfit(), add_risktable(), add_censor_mark(),
add_confidence_interval(), add_pvalue(), scale_ggsurvfit()), a label that
avoids overlaps (geom_text_repel()), markdown text (element_markdown())
and patchwork's arrangement (wrap_plots(), plot_layout(),
plot_annotation()); 2 sometimes; 1 rarely.  A function with no card is
named `pkg::fn` in a SPEC (`ggh4x::facet_manual`).

| Package | 2026-10-30 before | 2026-10-30 after |
|---|---:|---:|
| **extensions, weighted** | **58.9%** (106 yes / 240 partial / 5 no) | **100.0%** (351 yes) |
| ggsurvfit | 52.8% | 100.0% |
| ggrepel | 69.6% | 100.0% |
| ggh4x | 62.2% | 100.0% |
| ggtext | 77.0% | 100.0% |
| ggnewscale | 90.0% | 100.0% |
| patchwork | 45.6% | 100.0% |

ggplot2 itself stays at 100.0% (2303 arguments).

Added in this run:

- An extension's function without a card is its kind's, by `pkg::fn`
  (`.schema_ext_kind()`): ggh4x's facets, strips and element lists,
  ggtext's elements and geoms, ggsurvfit's `add_*()`, patchwork's
  arrangement; before, `ggh4x::facet_grid2` was checked by its formals
  only and `patchwork::plot_spacer()` was an error ("neither the schema
  nor its package has").
- Cards for 26 more functions: ggsurvfit's `add_legend_title()`,
  `add_risktable_strata_symbol()`, `stat_stepribbon()`,
  `scale_ggsurvfit()`, `theme_ggsurvfit_default()` / `_KMunicate()`,
  `theme_risktable_default()` / `_boxed()`; ggrepel's
  `position_nudge_repel()`; ggh4x's `facet_grid2()`, `facet_wrap2()`,
  `force_panelsizes()`, `strip_nested()` / `_themed()` / `_vanilla()`,
  `elem_list_text()` / `_rect()`; ggtext's `element_textbox()`,
  `geom_textbox()`; ggnewscale's `new_scale()`; patchwork's
  `plot_spacer()`, `wrap_elements()`, `inset_element()`, `guide_area()`,
  `area()`, `free()`.
- Typed by kind (`inst/fig/gg_args.csv`, 123 rows, new kinds `plot`,
  `ggsurvfit_layer`, `compose`): the risk table's `risktable_stats`,
  `stats_label`, `risktable_group`, `risktable_height`, `combine_groups`,
  `theme`; `add_pvalue()`'s `caption`, `location`, `rho`, `pvalue_fun`;
  `add_quantile()`'s `y_value` / `x_value`; ggrepel's `box.padding`,
  `point.padding`, `min.segment.length`, `force`, `max.overlaps`, `seed`
  ...; ggh4x's `independent`, `remove_labels`, `strip`, `nest_line`,
  `bleed`, `render_empty`; ggtext's `halign`, `padding`, `box.colour`,
  `width`, `orientation`; patchwork's `design`, `guides`, `axes`,
  `tag_levels`, `inset_element()`'s place and plot (an object by name).
  A card's shown `...` arguments are typed too (add_quantile's
  `linetype`, add_censor_mark's `shape`).
- `x_scales`, `y_scales` and `stats_label` are written `list(...)`.
- Fixes: ggrepel's `direction` is `both|y|x` (it was typed as
  geom_step's `hv|vh|mid`, so `direction: y` was an error); a ggsurvfit
  theme or `stat_stepribbon()` on a plain ggplot attaches ggsurvfit, a
  patchwork function inside a plot attaches patchwork.

What made the difference, round 1:

- Any function of a plot ggplot2 exports is written and checked by its
  formals; before, a function without a card (`scale_colour_brewer`,
  `theme_bw`, `guide_axis`, `coord_trans`) was an error of the check.
- A geom given `stat:` takes that stat's parameters (`pad` for ecdf;
  `fun`, `fun.data`, `fun.min`, `fun.max`, `fun.args` for summary; `level`,
  `type`, `segments` for ellipse), a stat given `geom:` that geom's
  (`stat_summary(geom = "errorbar", width = 0.2)`).
- `fun.args`, `method.args`, `args`, `dparams`, `override.aes` are written
  `list(...)` (before: `c(...)`) and read back as a map.
- A scale's `...` are its constructor's arguments (`scale_colour_manual(name
  =, labels =, limits =)` warned before); coord_fixed()'s are
  coord_cartesian()'s; coord_trans() of ggplot2 4.0 is coord_transform()'s.
- The shared arguments are typed and labelled once by kind
  (`inst/fig/gg_args.csv`: layer, scale, coord, facet, theme, element,
  guide, position, helper, labs); theme()'s settings each take its element
  (`axis.text.x` an element_text, not an element_line), the per-side
  variants (`axis.text.x.top`) as the setting they refine.
- Cards for 14 more geoms, the 5 stats with no geom twin (stat_ecdf,
  stat_summary, stat_summary_bin, stat_ellipse, stat_unique), 8 positions,
  35 scales, expansion / sec_axis / dup_axis / margin, 4 coords, xlab /
  ylab / ggtitle, 8 complete themes, element_text / line / rect / blank and
  6 guides.

Round 2:

- Cards for the 2-d layers (geom_contour / _filled, geom_density_2d /
  _filled, geom_bin_2d), geom_quantile, geom_spoke, geom_function,
  geom_blank, and the 11 stat_*() with a geom twin (stat_smooth, stat_bin,
  stat_count, stat_boxplot, stat_density, stat_ydensity, stat_identity,
  stat_function, stat_qq, stat_qq_line, stat_sum).
- Cards for 14 scales (scale_linewidth / _manual, scale_*_discrete /
  _continuous, scale_x_binned, scale_*_steps / steps2 / fermenter,
  scale_x_time) and 3 guides (guide_axis_logticks, guide_axis_theta,
  guide_axis_stack).
- Typed by kind (inst/fig/gg_args.csv, 106 more rows): ggplot2 4.0's look
  of a part of a box plot, a violin, a crossbar or a label
  (`whisker.colour`, `staple.linewidth`, `median.linetype`, `box.colour`,
  `quantile.linetype`, `border.colour`, `text.colour`, `size.unit` ...),
  the stats' parameters (`bounds`, `xseq`, `h`, `contour_var`, `args`,
  `xlim` ...), the rarer theme settings (`axis.minor.ticks.*`,
  `legend.frame`, `legend.ticks*`, `legend.box.*`, `legend.location`,
  `plot.tag.location`, ggplot2 4.0's `panel.widths` / `panel.heights`,
  `point`, `polygon`, `geom`, `spacing`, `margins`), the complete themes'
  `header_family` / `accent`, coord_radial()'s 4.0 names, labs()'s
  `dictionary`.
- Fix: `visible: [.., y]` / `[.., n]` read as TRUE / FALSE by YAML in
  tfl_text_column, tfl_ard_number, geom_function, stat_function (quoted).
