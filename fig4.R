#fig4
rm(list = ls())
setwd("C:/Users/Owner/Documents/replication")

library(haven)
df_final<-read_dta("Bursztyn_etal2020AER_rep/data/clean_data/02_follow_up_clean.dta")


df_fig4 <- df_final |> group_by(condition2) |>
  summarize(mean_number = mean(applied_out_fl)*100, 
            sd_number = sd(applied_out_fl*100)
  ) |>
  mutate(count_number = case_when(condition2 == 0 ~ 247, 
                                  TRUE ~ 253
  ), 
  condition3 = case_when(condition2 == 0 ~ "Control", 
                         TRUE ~ "Tratment")
  )



df_fig4 |> ggplot(aes(x = condition3, y = mean_number, group = condition3)) +
  geom_bar(stat = "identity",fill="lightskyblue") + 
  geom_errorbar(aes(ymax = mean_number + 1.96*sd_number/sqrt(count_number), 
                    ymin = mean_number - 1.96*sd_number/sqrt(count_number)), width=0.05) +
  theme_bw() +
  labs(x ="", y = "Percent Sign up") +
  geom_text(aes(label = paste0(round(mean_number, digits = 2), "%", sep =""), y = 10)) 
#  +geom_text(aes(label = "p-value ????"), y = 40)

