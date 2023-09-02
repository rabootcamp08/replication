#figure3
rm(list = ls())
setwd("C:/Users/Owner/Documents/replication")

df_final <- read.csv("df_final.csv")

df_fig3 <- df_final |> group_by(condition2) |>
  summarize(mean_number = mean(signed_up_number)*100, 
            sd_number = sd(signed_up_number*100)
            ) |>
  mutate(count_number = case_when(condition2 == 0 ~ 247, 
                                  TRUE ~ 253
                                  ), 
         condition3 = case_when(condition2 == 0 ~ "Control", 
                                TRUE ~ "Tratment")
         )






df_fig3 |> ggplot(aes(x = condition3, y = mean_number, group = condition3)) +
  geom_bar(stat = "identity",fill="lightskyblue") + 
  geom_errorbar(aes(ymax = mean_number + 1.96*sd_number/sqrt(count_number), 
                    ymin = mean_number - 1.96*sd_number/sqrt(count_number)), width=0.05) +
  theme_bw() +
  labs(x ="", y = "Percent Sign up") +
  geom_text(aes(label = paste0(round(mean_number, digits = 2), "%", sep =""), y = 10)) 
#  +geom_text(aes(label = "p-value ????"), y = 40)

