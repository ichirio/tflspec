# tfl_tpl_help(): the head, the tiers, its table invisibly

    Code
      tfl_tpl_help("demog", tier = c("required", "usual"))
    Output
      demog (table, layout summary; variant of summary; tflspec)
      Demographics
      Call: tfl_tpl(ard, "demog", ...)
      
      required (1):
        param      | effective (layer) | what
        population | - (default)       | the analysis set's id (SAF) [report: population, required]
      
      usual (6):
        param       | effective (layer)                                                            | what
        by          | TRT01A (fallback)                                                            | the column variable [study: settings.treatment, else TRT01A]
        continuous  | AGE (variant:demog)                                                          | the continuous variables, in order
        categorical | AGEGR1, SEX, RACE, ETHNIC (variant:demog)                                    | the categorical variables, in order
        labels      | AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ... (variant:demog) | each variable's heading; given by name, merged (RACE: ~ drops one)
        cells       | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}), {mean:.1f... (fallback)      | the rows of each continuous variable [study: plan_cells.continuous, else n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}), {mean:.1f...]
        total       | FALSE (template)                                                             | TRUE: a Total column (each analysis again without `by`, plan_total())

