# tfl_tpl_help(): the head, the tiers, its table invisibly

    Code
      tfl_tpl_help("demog", tier = c("required", "usual"))
    Output
      demog (table, layout summary; tflspec)
      Demographics
      Call: tfl_tpl(ard, "demog", ...)
      
      required (3):
        param       | effective (layer) | what
        continuous  | - (template)      | the continuous variables, in order (required: this or `categorical`)
        categorical | - (template)      | the categorical variables, in order (required: this or `continuous`)
        by          | - (template)      | the column variable (the arms); not a default and not the study's treatment
      
      usual (4):
        param   | effective (layer)                                                       | what
        formats | N = 0; mean = 1; sd = 2; median = 1; min = 0; max = 0; p ... (template) | the ARD's formats (precision): per statistic, and per variable (BMIBL: {mean: 2, sd: 3}); given by name, merged; a number is the decimals, "xx.x%" a proportion as a percent
        total   | FALSE (template)                                                        | TRUE: a Total column, last (each analysis again without `by`)
        labels  | AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ... (template) | each variable's heading; given by name, merged (RACE: ~ drops one); a variable it does not name has none; unset: no headings
        cells   | n = {N}; Mean (SD) = {mean} ({sd}), {mean} (-); Median = ... (fallback) | the rows of each continuous variable: where each statistic goes (placement only, {mean}) [study: plan_cells.continuous, else n = {N}; Mean (SD) = {mean} ({sd}), {mean} (-); Median = ...]

