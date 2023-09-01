# table2

library(estimatr)

output_base <- lm_robust(signed_up_number ~ condition2,
                         data = df_final)

output_fe <- lm_robust(signed_up_number ~ condition2
                       + as.factor(df_final$session),  
                       data = df_final)

output_baseline_belief <- lm_robust(signed_up_number ~ condition2
                                    + as.factor(session)
                                    + mwage_self + mwage_others
                                    + outside_self + outside_others
                                    + semiseg_self + semiseg_others,
                                    data = df_final)

output_control <- lm_robust(signed_up_number ~ condition2
                            + as.factor(session)
                            + mwage_self + mwage_others
                            + outside_self + outside_others
                            + semiseg_self + semiseg_others
                            + employed_wife + employed_now
                            + as.factor(education) + children
                            + num_know_per + num_mfs_per
                            + age,
                            data = df_final)

library(modelsummary)

#library(fwildclusterboot)
# output_base_wildboot <- boottest(
#   output_base,
#   clusterid = "session",
#   parameter = "condition2",
#   B = 1000
# )

msummary(
  list(output_base, output_fe, output_baseline_belief, output_control),
  coef_map = c("condition2" = "Treatment",
               "(Intercept)" = "Constant" ),
  statistic = c("p-value: robust SE = {p.value}"),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.")
