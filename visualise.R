#可視化セクション

df_final <- read.csv("df_final.csv")

df_final <- df_final|>
mutate(outside_guess_share = outside_guess / 30)

df_raw_02 <- df_raw_02 |>  
  mutate(outside_others_truth = outside_objective / 30)

df_final <- df_final |>
  left_join(df_raw_02, by="session") |>
  select(-c(outside_objective.y, semiseg_objective.y, mwage_objective.y))

df_final <- df_final |> 
mutate(outside_wedge_share = (outside_wedge / 30) * 100)

df_final|>
  ggplot(aes(x=outside_wedge_share, y=after_stat(density)))+
  geom_histogram(binwidth = 10, boundary = 10)

summary(df_final)

df_final|>
  group_by(condition2)|>
  tally()->df

df_final|>
  group_by(condition2)|>
  summarise(Number_of_Children = mean(children),Sd_Num_Child=sd(children),Age=mean(age,na.rm = TRUE),Sd_Age=sd(age,na.rm=TRUE))->df1

df_controled <- df_final |>
  group_by (condition2) |>
  summarize(across(c(age, college_deg, children,employed_now, employed_wife, 
              num_know_per, num_mfs_per), ~ mean(.x, na.rm  = TRUE),
            .names = "mean_{col}"),
            across (c(age, college_deg, children,employed_now, employed_wife, 
                      num_know_per, num_mfs_per), ~ sd(.x, na.rm  = TRUE),
                    .names = "sd_{col}")
            )

df_all <- df_final |>
  summarize(across(c(age, college_deg, children,employed_now, employed_wife, 
                     num_know_per, num_mfs_per), ~ mean(.x, na.rm  = TRUE),
                   .names = "mean_{col}"),
            across (c(age, college_deg, children,employed_now, employed_wife, 
                      num_know_per, num_mfs_per), ~ sd(.x, na.rm  = TRUE),
                    .names = "sd_{col}")
  )

df_all$condition2 <- 2 select()

df_all <- df_all |>
  select(condition2, mean_age:sd_num_mfs_per)

df_summary <- rbind(df_controled, df_all) |>
  pivot_longer(!condition2) |>
  pivot_wider(names_from = condition2, values_from = value) |>
  select(name, All = "2", Control = "0", Treatment = "1") 

#テスト作業（クリーンデータ）
# df_clean1 <- df_clean|>
#   mutate(outside_guess_share = outside_guess / 30)
# 
# df_clean2 <- read_dta("C:/Users/Owner/Documents/replication/Bursztyn_etal2020AER_rep/data/clean_data/02_follow_up_clean.dta")
# 
# df_clean2 <- df_clean2 |>  
#   mutate(outside_others_truth = outside_objective / 30)
# 
# df_clean_final <- df_clean1 |>
#   left_join(df_clean2, by="session") 
# 
# df_final <- df_final |> 
#   mutate(outside_wedge_share = (outside_wedge / 30) * 100)
# 
# df_clean|>
#   ggplot(aes(x=outside_wedge_share, y=after_stat(density)))+
#   geom_histogram(binwidth = 10, boundary = 10)
