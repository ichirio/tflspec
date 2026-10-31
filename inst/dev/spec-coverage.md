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
catalog (`inst/ard/functions.csv`, `args.csv`, `passed_args.csv`).  Scope:
every exported `ard_*()` -- its own arguments and, since 2026-10-30, the
arguments it passes on through `...` to the function doing the work
(`ard_regression(exponentiate =)` to `broom.helpers::tidy_plus_plus()`,
`ard_car_anova(type =)` to `car::Anova()`, `ard_stats_t_test(var.equal =)`
to `stats::t.test()` ...: 55 arguments of 18 functions) -- and cards' helpers
a row writes in `post` (`add_calculated_row()`, `filter_ard_hierarchical()`,
`sort_ard_hierarchical()`, `diff_ard_hierarchical()`,
`replace_null_statistic()`, `update_ard_stat_label()` and, since
2026-10-31, `update_ard_fmt_fun()`).

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

cards 0.9.0, cardx 0.3.4; 61 functions, 368 arguments (313 their own, 55
passed on).

| Scope | 2026-10-29 before | 2026-10-29 after | 2026-10-30 before (wider scope) | 2026-10-30 after | 2026-10-31 before (+ update_ard_fmt_fun) | 2026-10-31 after |
|---|---:|---:|---:|---:|---:|---:|
| **cards / cardx, weighted** | **75.9%** (153 yes / 155 partial) | **99.3%** (304 / 4) | **93.0%** (304 / 59) | **99.3%** (359 / 4) | **99.0%** (362 / 6) | **100.0%** (368 / 0) |
| cards ard_*() | 79.9% | 99.4% | 99.4% | 99.4% | 99.4% | 100.0% |
| cardx ard_*() | 76.0% | 99.8% | 89.3% | 99.8% | 99.8% | 100.0% |
| cards post helpers | 63.9% | 96.1% | 96.1% | 96.1% | 94.2% | 100.0% |
| weight-3 functions only | 75.1% | 99.6% | 93.1% | 99.6% | 99.6% | 100.0% |

(2026-10-30 before: the tflspec of `next` measured on the wider scope -- a
passed-on argument could be written as R in `args` and reached the
function, but the catalog did not describe it and its value was not
judged: partial.  2026-10-31 before: the same, with
`update_ard_fmt_fun()` in the `post` scope -- written as R in `post`, its
names checked, its values not judged.)

### Added 2026-10-31

* The last values taken as R are judged by their shape:
  * `ard_categorical_ci(weights = )` (the stratified Wilson weights): numbers
    between 0 and 1 that add up to 1, one a stratum (`c(0.5, 0.3, 0.2)`);
  * `ard_pairwise(include = )`: a vector of the levels compared
    (`c("Placebo", "Drug A")`), not a list or a formula;
  * `add_calculated_row(fmt_fun = )`: a whole number of decimals (`2L`), a
    format cards takes (`"xx.x"`, `"xx.x%"`: an `x`, then `x`'s, one point at
    most, `%` only at the end) or a function (`cards::label_round(1)`);
  * `replace_null_statistic(value = )`: one value (`NA`, `0`, `"NE"`).
  A call that makes the value (`rep(0.25, 4L)`) or a name is the program's.
* `update_ard_fmt_fun()` is a described `post` step (`variables`,
  `stat_names`, `fmt_fun`, `filter`): a format for some rows only, e.g. one
  arm's mean with 3 decimals (`filter = group1_level == "Placebo"`).  Its
  format is no longer lost: a `post` step that sets formats now runs after
  the format step (`fmt_ard()`, which gives every statistic its default) and
  the ARD is formatted again, `cards::apply_fmt_fun(replace = TRUE)`.
  The same for `add_calculated_row(stat_name = "se", fmt_fun = 4L)`: `se`
  has a default format, which used to win; its format is now said again
  after the format step (`update_ard_fmt_fun(stat_names = "se", fmt_fun =
  4L)`).
* `?tfl_ard_args` says which shapes a `code` argument's value is checked by.

### Added 2026-10-30

* The arguments a cardx function passes on through `...`, described and
  their values judged (`inst/ard/passed_args.csv`, `tfl_ard_args()`'s new
  `passed_to` column; a company adds rows with
  `options(tflspec.ard_passed_args =)`):
  * `ard_regression()` / `ard_regression_basic()` to
    `broom.helpers::tidy_plus_plus()`: `exponentiate` (hazard, odds and rate
    ratios), `conf.level`, `conf.int`, `intercept`, `add_reference_rows`,
    `add_header_rows`, `add_n`, `show_single_row`, `include`;
  * `ard_car_anova()` to `car::Anova()`: `type` ("II" / "III", or 2 / 3:
    a number among the choices is taken as a number), `test.statistic`;
  * the tests to `stats`: `alternative`, `mu`, `var.equal` (t tests, one-way
    ANOVA), `exact` / `correct` (Wilcoxon, CMH, McNemar, chi-square, the
    test of proportions), `simulate.p.value` / `B` (chi-square, Fisher), `r`
    (Poisson).
* The tests run them: a 2 x 2 chi-square with `correct = FALSE` is
  `chisq.test(correct = FALSE)`'s p-value, and a Cox model's row with
  `exponentiate = TRUE` gives `exp(coef(coxph()))`.

### Where the gaps are

The scope is close to its ceiling; what is left and what is next:

Every argument in scope is described and its value judged.  What is left:

1. A `post` step of the study's own or of another package that sets formats
   is not moved: the format step after it gives the defaults.
2. A `custom` row's code is run as it is: nothing in it is judged.
3. A passed-on argument's name is not checked (a function that passes `...`
   on takes any name); the catalog names the ones clinical tables use, not
   every argument of the function they reach (`survival::survdiff()`,
   `smd::smd()`, `stats::aov()`'s are not described).
4. Not measured: the helpers the generated code writes itself
   (`continuous_summary_fns()` from the statistics column and the statistics
   catalog's own functions, `label_round()` from the formats column --
   `xx.x` sets the decimals, the width is rtfreporter's alignment's,
   `apply_fmt_fun()`).  A denominator of numbers is the denominator column
   (`86`, `c("Drug X" = 86, "Placebo" = 85)`); a data frame written in
   `args` is the program's.

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
  assemblers, the validation kit, `plan_apply()` (`rtf_tables()` takes the
  plan itself), deprecated and superseded arguments, and -- since
  2026-10-31 -- rtfreporter's own deprecated exports
  (`rtfreporter:::.deprecated_exports`: the old border constructors,
  `paginate()`, `assemble_toc()` ..., removed in 0.9.0 and left out of
  rtfreporter's own API measure) -- no SPEC item by design.  `[out of scope]`: `plan_layers()`, designed in another session.

Percentage: yes counts 1, partial 0.5.

| Run        | All arguments | SPEC-relevant (helpers out) | Functions yes / partial / no |
|------------|-------------------|----------------------------------|------------------------------|
| 2026-10-28 before | 41.9% (287 / 30 / 403 of 720) | 47.5% (287 / 30 / 319) | 38 / 7 / 90 |
| 2026-10-28 after  | 43.3% (300 / 24 / 396 of 720) | 49.1% (300 / 24 / 312) | 40 / 6 / 89 |
| 2026-10-29 before | 43.7% (305 / 25 / 397 of 727) | 49.6% (305 / 25 / 310 of 640) | 41 / 6 / 90 |
| 2026-10-29 after  | 43.8% (306 / 25 / 396 of 727) | 49.8% (306 / 25 / 309 of 640) | 41 / 6 / 90 |
| 2026-10-30 before | 43.8% (306 / 25 / 397 of 728) | 49.7% (306 / 25 / 310 of 641) | 41 / 6 / 90 |
| 2026-10-30 after  | 44.3% (310 / 25 / 393 of 728) | 50.3% (310 / 25 / 306 of 641) | 43 / 5 / 89 |
| 2026-10-31 before | 44.3% (310 / 25 / 393 of 728) | 50.3% (310 / 25 / 306 of 641) | 43 / 5 / 89 |
| 2026-10-31 after  | 45.4% (310 / 41 / 377 of 728) | 57.2% (310 / 41 / 227 of 578) | 43 / 5 / 89 |

(2026-10-31: the measure's corrections only, nothing new a SPEC writes --
rtfreporter's deprecated exports and the superseded `rtf_section(secinfo =)`
/ `generate_rtfreport(report =)` are helpers (63 arguments out of the
SPEC-relevant count); `rtf_table_style()`'s 15 fields are said by
`plan_style()`'s own arguments, `rtf_tables(col_spec = )` by
`plan_cell_style(cols =)` and `generate_rtfreport(program =,
program_fallback =)` by `rtf_document()`, which tflspec fills: partial.)

(2026-10-30 before is rtfreporter@next with one more argument,
`fmt_value_paren(rounding =)`.)

(2026-10-29 before is rtfreporter@next with 7 more arguments --
`plan_append()`, `table_plan(value = )`, `rtfreporter_validation_kit()` --
and the measure's corrections: `rtf_tables(style = )` is said by
`plan_style()`'s `rtf_table_style()` fields (partial), `read_gt` is
deprecated, the validation kit a helper.)

### Added 2026-10-30

* A border's side with a look of its own: `rtf_border_line()` (`style`,
  `width`, `color`) in the schema, a side of `rtf_border()` taking
  `{fn: rtf_border_line, args: {style: double, width: 30}}`; `rtf_border()`'s
  `all`, `inside_h` and `inside_v` typed; `plan_style(border =)` takes an
  `rtf_border()` call besides "tfl" / "none".
* The schema check looks inside a call a value holds: a wrong style in
  `{fn: rtf_border, args: {top: {fn: rtf_border_line, args: {style: wavy}}}}`
  is said as `rtf_border(top = ): rtf_border_line(style = ) is ...` (table and
  listing items; the figure items are checked as before).
* `rtf_document(color_table =)`: `page.rtf_document.color_table`, passed on.

### Where the gaps are

Most "no" rows are rtfreporter's lower-level table API -- `rtftable()`,
`widen_ard()`, `as_rtftable(s)()`, the `style_*()`, `set_*()`, `fmt_*()`,
`paginate*()` and `listing_*()` helpers -- which the plan verbs already
stand for: a SPEC says the plan, not these calls.  The gaps that a SPEC
could usefully close next:

1. `rtf_section(page =)`: a section of its own header from a page on (a
   report of several objects, `contents:`, could give each its own).
2. `rtfplot(width_twips =, height_twips =)` (the figure SPEC's).
3. `rtf_table_style()` as a value (`plan_style(style = )` of a company's
   own style object): its fields are said one by one today.

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
Next: the extension packages (ggsurvfit, ggh4x, ggrepel, ggtext,
ggnewscale, patchwork), and the scope's edge (geom_sf, geom_hex, the
rarer stat_*()).

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
