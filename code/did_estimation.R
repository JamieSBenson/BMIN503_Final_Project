### Diff-in-diff estimation
estimate_diff <- function(
  buffer_size,
  outcome,
  density,
  cluster,
  change_pt,
  df,
  control = "notyettreated"
) {
  buffer_size <- set_units(buffer_size, m)
  df <- df %>% filter(buffer == buffer_size)
  df$outcome <- df[[outcome]]

  if (density == TRUE) {
    df$outcome_density <- df$outcome / df$area
    df$outcome <- df$outcome_density
  }

  # Creates one version of the model with clustered standard errors at the clinic level...
  attgt_ods <- att_gt(
    yname = outcome,
    tname = "year",
    idname = "group",
    allow_unbalanced_panel = TRUE,
    #panel = FALSE,
    clustervars = cluster,
    gname = change_pt,
    xformla = ~1,
    data = df,
    control_group = control
  )

  group_effects <- aggte(
    attgt_ods,
    type = "dynamic",
    na.rm = TRUE,
    clustervars = cluster
  )

  estimate <- data.frame(
    estimate = group_effects$overall.att,
    se = group_effects$overall.se,
    ci_low = group_effects$overall.att - 1.96 * group_effects$overall.se,
    ci_high = group_effects$overall.att + 1.96 * group_effects$overall.se
  ) %>%
    mutate(
      buffer = buffer_size,
      outcome = outcome,
      density = density,
      direction = ifelse(change_pt == "first_open", "Opening", "Closure")
    )

  return(estimate)
}
