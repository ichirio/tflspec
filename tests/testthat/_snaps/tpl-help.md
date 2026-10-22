# tfl_tpl_help(): the head, the tiers, its table invisibly

    Code
      tfl_tpl_help("demog", tier = c("required", "usual"))
    Output
      demog (table, layout summary; variant of summary; tflspec)
      Demographics
      Call: tfl_tpl(ard, "demog", ...)
      
      required (1):
        param      | effective (layer) | what
        population | - (template)      | the analysis set's id (SAF)
      
      usual (6):
        param       | effective (layer)                                                            | what
        by          | TRT01A (template)                                                            | the column variable
        continuous  | AGE (variant:demog)                                                          | the continuous variables, in order
        categorical | AGEGR1, SEX, RACE, ETHNIC (variant:demog)                                    | the categorical variables, in order
        labels      | AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ... (variant:demog) | each variable's heading; given by name, merged (RACE: ~ drops one)
        cells       | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}); Median = ... (template)      | the rows of each continuous variable
        total       | FALSE (template)                                                             | TRUE: a Total column (each analysis again without `by`, plan_total())

