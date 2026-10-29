# SPEC coverage

How much of each package the SPEC (a report's `content:` items, checked by
`tfl_schema_check()`, catalogued by `inst/spec/schema.yml`) can write.

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
