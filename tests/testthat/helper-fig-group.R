# A figure template's group as tfl_fig_template() fell back to it before a
# figure's group had no default (PRINCIPLES 1.14): the arms the preset drew
# (TRT01P: km, forest, bar; TRT01A: individual, mean, box, scatter, pk).
# NULL for a template that draws no arms (one arm, the spider, the
# waterfalls, the swimmers, a whole script).  The tests write the value the
# fallback gave, so what they draw does not change.
fig_group_of <- function(template) {
  tp <- tfl_fig_templates()
  at <- tp$template == template
  if (!any(at) || !tp$parts[at] || template %in% c("km_single_arm", "individual_spider")) return(NULL)
  switch(tp$kind[at], km = , forest = , bar = "TRT01P",
         individual = , mean = , box = , scatter = , pk = "TRT01A", NULL)
}
