# What each column of a spec workbook means

The help the spec workbooks carry as comments on their header cells, as
one table: every column of every sheet – ARD, table, report and listing
– with the form of its value, its unit and what a blank cell means, and
an example. A row with no `column` describes the sheet; rows whose
`sheet` is in parentheses are about the whole workbook (`output_id`,
`note`, the tokens).

## Usage

``` r
tfl_spec_columns(sheet = NULL)
```

## Arguments

- sheet:

  Sheet names to keep the rows of, or `NULL` for every row. A row naming
  several sheets (`"titles / footnotes"`) is kept for each.

## Value

A data frame: `sheet`, `column`, `description`, `example`.

## Examples

``` r
head(tfl_spec_columns("tables"))
#>    sheet column
#> 1 tables   <NA>
#> 2 tables   cols
#> 3 tables   rows
#> 4 tables  label
#> 5 tables  stats
#> 6 tables  value
#>                                                                                                                                                                                                                                                                                                                                                                                         description
#> 1                                                                                                                                                                                                                                                                             Table: one row a report. Its roles (table_plan()) and the table-wide options; a verb written in code afterwards wins.
#> 2                                                                                                                                                                                                                                                                                                         The key spread across the columns, outermost first, | between them (table_plan(cols = )).
#> 3                                                                                                                                                                                                                                                                        The keys down the rows (table_plan(rows = )); name = column renames, a quoted value is a constant heading; | between them.
#> 4                                                                                                                                                                                                                                                                   Where the row label comes from (table_plan(label = )); blank: .label; NA: used to tell rows apart, not printed; NULL: not used.
#> 5                                                                                                                                                                                                                                                                            cells (cells built from templates) or rows (one statistic a row, the raw values) (plan_cells(stats = )); blank: cells.
#> 6 Which of the ARD's values the table prints: stat (the number, rounded here: the digits sheet, a template's {x:.1f}) or stat_fmt (the ARD's own text, as it was formatted: no digits to set; a template still combines them, {mean} ({sd})); a row may say otherwise in its template ({mean:stat_fmt}, {mean:.1f}). With stats = rows: which goes in the cell (plan_cells(value = )). Blank: stat.
#>                                                  example
#> 1                                                   <NA>
#> 2                                             TRTA | SEX
#> 3 group1 = AEBODSYS | grp = "Worst Post-Baseline Values"
#> 4                                        label = AEDECOD
#> 5                                                   rows
#> 6                                                   stat
tfl_spec_columns("analyses")
#>       sheet        column
#> 1  analyses          <NA>
#> 2  analyses   analysis_id
#> 3  analyses        parent
#> 4  analyses         label
#> 5  analyses        method
#> 6  analyses          data
#> 7  analyses       dataset
#> 8  analyses       records
#> 9  analyses population_id
#> 10 analyses         where
#> 11 analyses            by
#> 12 analyses       overall
#> 13 analyses        strata
#> 14 analyses        groups
#> 15 analyses     variables
#> 16 analyses      optional
#> 17 analyses    statistics
#> 18 analyses   denominator
#> 19 analyses       formats
#> 20 analyses          args
#> 21 analyses          post
#> 22 analyses          code
#> 23 analyses       purpose
#> 24 analyses        reason
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     description
#> 1                                                                                                                                                                                                                                                                                                                                             ARD: one analysis a row; each becomes one cards / cardx call in the ARD program, its result tagged with output_id, analysis_id and population_id.
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                           The analysis's id within its report; an ARD column.
#> 3                                                                                    The analysis this one runs inside: an analysis of the same report whose method is cards::ard_stack (several analyses on the same data and by, with the by counts and the total N), cards::ard_strata (within subgroups) or cards::ard_pairwise (each pair of groups). It takes the parent's dataset, population_id and where (and in a stack its by), so leave those blank; blank: an analysis of its own.
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                     A name a person reads (for ARS and the GUI); blank: none.
#> 5                                                                                                                                                                                                                                                                                         What is computed: a keyword of tfl_ard_methods() (continuous, categorical, hierarchical ...), any pkg::function (cards::, cardx::), or a function of the study's own that the study key source loads.
#> 6                                                                                                                                                                                                                                                                                                                     The analysis data analysed (a data_id of analysis_data), instead of dataset and population_id: the population is its population; blank: dataset and population_id say it.
#> 7                                                                                                                                                                                                                                                                                                                                                                                              The data analysed; blank: the population's own dataset. Blank when `data` or `records` is given.
#> 8                                                                                                                                                                                                                                                                                                                               A set of the records sheet (record_id): its dataset and condition, instead of dataset and where (the analysis's where on top); blank: dataset and where say it.
#> 9                                                                                                                                                                                                                                                                                                                                                              The population: the data is restricted to its subjects; blank: no restriction (population is NULL), or the population of `data`.
#> 10                                                                                                                                                                                                                                                                                                                                                                  An R condition on the data, applied after the population (on an analysis data: after its own conditions); blank: every row.
#> 11                                                                                                                                                                                  Grouping variables (cards' by), | between them. In a table with columns (table_plan(cols = TRT01A)) every analysis has them -- blank is a problem, but for the columns' own count (the N) -- and (none) (YAML by: []) says an analysis has no columns. A variable the data does not have stops the program.
#> 12                                                                                                                                                                                                                                                                    TRUE: the analysis again without its by, over all its subjects (cards' overall rows, no group): what a table shows as its Total column (tables$total). On a cards::ard_stack row: .overall = TRUE. Blank: by groups only.
#> 13                                                                                                                                                                                                                                                                                                                                                                    Variables the analysis is repeated within (cards' strata: a subgroup, a parameter by visit), | between them; blank: none.
#> 14                                                                                                                                                                                                                                                                                                                                                                       Groups of the groups sheet the analysis runs in, each level once (group2 / group2_level), | between them; blank: none.
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                   The variables analysed (cards' variables), | between them.
#> 16                                                                                                                                                                                                                                                                                                   TRUE: the variables are the ones the data has of them (written any_of(); tfl_build_ard() says which it left out), as a template's default list. Blank: every variable must be in the data.
#> 17                                                                                                                                                                                                                                                                              For continuous / categorical / missing: the statistics computed, in order (tfl_ard_statistics()); for other methods: the statistics kept of what the method gives; | between them; blank: the method's default.
#> 18                                                       What percentages are of: population (hierarchical and max take it anyway), row / column / cell (cards), another population, a dataset (its records of the population's subjects), an analysis data (as it is: one row a subject and phase gives the N of each phase), or numbers: one (86, every column's N) or one a value of the by (c("Drug X" = 86, "Placebo" = 85); YAML {Drug X: 86, Placebo: 85}); blank: the method's default.
#> 19                                                                                                                                                                                                                    Display formats, statistic=format, | between them; VAR:statistic=format for one variable. xx.x = one decimal (the x's count only after the point), xx.x% = a proportion as a percent, a number = decimals, pvalue = <0.001 or 3 decimals; blank: the statistic's default.
#> 20                                                                                                                                                                                                                                                                         More arguments of the call, as R (data and population are bound), in any order; refused if not R. An argument a column gives (by, variables, strata, denominator, statistic) may not be given here too; blank: none.
#> 21 Steps on the ARD after the call, each a call with the ARD left out, | between them: cards::add_calculated_row() (a statistic computed from others), cards::filter_ard_hierarchical() (only the rows that pass), cards::sort_ard_hierarchical(), cards::diff_ard_hierarchical(), complete_hierarchy() (a code list's values no subject has, counted 0: written with the row's variables and the code lists' `under`), or a function of your own that takes an ARD and gives one; blank: none.
#> 22                                                                                                                                                                                                                                                                                                                                                                                                         method = custom only: the R code that gives the ARD (data and population are bound).
#> 23                                                                                                                                                                                                                                                                                                                                                                                    For CDISC ARS (tfl_ars()): PRIMARY / SECONDARY / EXPLORATORY OUTCOME MEASURE; blank: tfl_ars(purpose = ).
#> 24                                                                                                                                                                                                                                                                                                                                                             For CDISC ARS: SPECIFIED IN PROTOCOL / SPECIFIED IN SAP / DATA DRIVEN / REQUESTED BY REGULATORY AGENCY; blank: SPECIFIED IN SAP.
#>                                                             example
#> 1                                                              <NA>
#> 2                                                               AGE
#> 3                                                              DEMO
#> 4                                                       Age (years)
#> 5                                                        continuous
#> 6                                                         adae_teae
#> 7                                                              ADAE
#> 8                                                             TESAE
#> 9                                                               SAF
#> 10                                                   TRTEMFL == "Y"
#> 11                                                           TRT01A
#> 12                                                             TRUE
#> 13                                                 PARAMCD | AVISIT
#> 14                                                     AGEGRP | SEX
#> 15                                                      AGE | BMIBL
#> 16                                                             TRUE
#> 17                                                    N | mean | sd
#> 18                                                              row
#> 19                                              mean=xx.x | p=xx.x%
#> 20                                            over_variables = TRUE
#> 21 cards::add_calculated_row(expr = sd / sqrt(N), stat_name = "se")
#> 22                       cards::ard_tabulate(data, variables = SEX)
#> 23                                          PRIMARY OUTCOME MEASURE
#> 24                                                 SPECIFIED IN SAP
```
