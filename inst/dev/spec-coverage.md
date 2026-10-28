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

| Scope | Before | After |
|---|---:|---:|
| **ggplot2: geoms (with their stats), scales, coords, themes, facets, guides, labs, positions** | **24.7%** | **94.9%** |
| geom_*() | 55.9% | 91.4% |
| a geom with another stat (geom_step(stat = "ecdf"), geom_pointrange(stat = "summary") ...) | 0.0% | 100.0% |
| scale_*() and expansion() / sec_axis() / dup_axis() | 8.1% | 95.9% |
| coord_*() | 34.5% | 98.8% |
| theme(), complete themes, element_*(), margin() | 5.0% | 91.2% |
| facet_wrap() / facet_grid() | 57.4% | 100.0% |
| guides() and guide_*() | 10.7% | 97.3% |
| labs(), xlab(), ylab(), ggtitle(), annotate() | 38.8% | 97.4% |
| position_*() | 0.0% | 100.0% |
| **stat_*()** (shown apart) | **44.2%** | **73.1%** |

Arguments measured: 2015 (after: 1533 yes, 482 partial, 0 no; before: 73
yes, 960 partial, 982 no).

What made the difference:

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

Still partial (no card or untyped): ggplot2 4.0's per-part styling of box
plots, violins, crossbars and labels (`whisker.colour`, `median.linewidth`,
`border.colour` ...), the rarer theme settings (`legend.ticks`,
`legend.box.*`, `axis.minor.ticks.*`, `panel.widths`), the stat_*() that
have a geom twin (stat_smooth = geom_smooth, stat_bin = geom_histogram ...)
and 2-d / contour layers.  Extension packages are measured later.
