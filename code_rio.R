library(tidyverse)
library(haven)
library(ggplot2)
library(modelsummary)
library(kableExtra)


###########################################################
# data cleaning
###########################################################


df_raw_01    <- read_dta("/Users/rio/Downloads/Bursztyn_etal2020AER_rep/data/raw_data/01_main_exp_raw1.dta")
df_clean_01 <- read_dta("/Users/rio/Downloads/Bursztyn_etal2020AER_rep/data/clean_data/01_main_exp_clean.dta")

# create relevant variables
df_raw_01 <- df_raw_01 %>%
  rename(labor_demand_guess = "q45_1_p2") %>%
  mutate(labor_demand_guess = case_when(!is.na(labor_demand_guess) ~ labor_demand_guess,
                                        is.na(labor_demand_guess) & !is.na(q45_4_p2) ~ q45_4_p2,
                                        is.na(labor_demand_guess) & is.na(q45_4_p2) & !is.na(q46_1_p2) ~ q46_1_p2,
                                        is.na(labor_demand_guess) & is.na(q45_4_p2) & is.na(q46_1_p2) & !is.na(q43_1_p2) ~ q43_1_p2),
         signed_up_number   = "sign_up",
         signed_up_number   = ifelse(no_wife_number == 1, 0, signed_up),
         condition2         = "condition",
         condition2         = ifelse(is.na(condition), 0, condition),
         age                = ifelse(between(age, 18, 36), age, NA),
         college_deg        = ifelse(education >= 6, 1, 0),
         married            = ifelse(marital == 2, 1, 0),
         num_know_per       = num_know/30,
         num_mfs_per        = num_mutual_friends/30,
         outside_guess      = outside_others + outside_self,
         semiseg_guess      = semiseg_others + semiseg_self,
         mwage_guess        = mwage_others + semiseg_self) 

## wedge
df_session_agg <- df_raw_01 %>%
  group_by(session) %>%
  summarize(outside_objective = mean(outside_self, na.rm = T)*30,
            semiseg_objective = mean(semiseg_self, na.rm = T)*30,
            mwage_objective   = mean(mwage_self, na.rm = T)*30) %>%
  ungroup()

df_raw_01 <- left_join(df_raw_01, df_session_agg, by = "session")

df_raw_01 <- df_raw_01 %>%
  mutate(outside_wedge = outside_guess - outside_objective,
         semiseg_wedge = semiseg_guess - semiseg_objective,
         mwage_wedge   = mwage_guess - mwage_objective,
         outside_wedge_pos = ifelse(outside_wedge > 0, 1, 0),
         interaction = condition2*outside_wedge_pos)

# drop the following unnecessary columns. note that since i did not create stag, # of columns is 35. 
df_raw_01_final <- df_raw_01 %>%
  select(-c(condition, marital, employed_ever, num_know, num_mutual_friends, q45_4_p2,
            q46_1_p2, q43_1_p2, count, condition_txt, glowork_choice, second_order_total, 
            signed_up, no_wife_number)) 

#write.csv(df_raw_01_final, "/Users/rio/Downloads/Bursztyn_etal2020AER_rep/01_main_exp_clean.csv", row.names = F)



###########################################################
# figure 2
###########################################################


df <- read.csv("/Users/rio/Downloads/Bursztyn_etal2020AER_rep/01_main_exp_clean.csv")


df <- df %>%
  mutate(outside_guess_share = outside_guess/30,
         outside_wedge_share = (outside_wedge / 30)*100)

df_session_agg_2 <- df_session_agg %>%
  mutate(outside_others_truth = outside_objective/ 30) %>%
  select(session, outside_others_truth)

df <- left_join(df, df_session_agg_2, by = "session")


# figure 2
fig2 <- df %>%
  ggplot(data = df, mapping = aes(x = outside_wedge_share, y=after_stat(density))) +
  geom_histogram(binwidth = 10, boundary = 0, alpha = 0.3, color = "lightseagreen", fill = "lightseagreen") + 
  geom_vline(xintercept =0, color = "orange2") +
  scale_x_continuous(breaks = seq(-100, 100, by = 10),
                     label  = seq(-100, 100, by = 10),
                     limits = c(-100, 100)) +
  scale_y_continuous(limits = c(0, 0.015)) +
  labs(x = "Wedge (guess % - objective %)", y = "Density", title = "Replication of Figure 2") +
  theme_bw()


###########################################################
# table 1
###########################################################


df_treat_control <- df %>%
  group_by(condition2) %>%
  summarise(across(c(age, children, college_deg),
                   ~ round(mean(.x, na.rm = TRUE), digits = 2), 
                   .names = "mean_{col}"),
            across(c(employed_now, employed_wife, num_know_per, num_mfs_per),
                   ~ round(mean(.x, na.rm = TRUE)*100, digits = 2), 
                   .names = "mean_{col}"),
            across(c(age, children, college_deg),
                   ~ round(sd(.x, na.rm = TRUE), digits = 2), 
                   .names = "sd_{col}"),
            across(c(employed_now, employed_wife, num_know_per, num_mfs_per),
                   ~ round(sd(.x, na.rm = TRUE)*100, digits = 2), 
                   .names = "sd_{col}")) %>%
  select(condition2, mean_age, sd_age, mean_college_deg,  
         mean_children, mean_employed_now,
         mean_employed_wife, mean_num_know_per, sd_num_know_per, 
         mean_num_mfs_per, sd_num_mfs_per)

df_all <- df %>%
  summarise(across(c(age, children, college_deg),
                   ~ round(mean(.x, na.rm = TRUE), digits = 2), 
                   .names = "mean_{col}"),
            across(c(employed_now, employed_wife, num_know_per, num_mfs_per),
                   ~ round(mean(.x, na.rm = TRUE)*100, digits = 2), 
                   .names = "mean_{col}"),
            across(c(age, children, college_deg),
                   ~ round(sd(.x, na.rm = TRUE), digits = 2), 
                   .names = "sd_{col}"),
            across(c(employed_now, employed_wife, num_know_per, num_mfs_per),
                   ~ round(sd(.x, na.rm = TRUE)*100, digits = 2), 
                   .names = "sd_{col}")) %>%
  mutate(condition2 = "all") %>%
  select(condition2, mean_age, sd_age, mean_college_deg,  
         mean_children, mean_employed_now,
         mean_employed_wife, mean_num_know_per, sd_num_know_per, 
         mean_num_mfs_per, sd_num_mfs_per)


df_summary_stat <- rbind(df_all, df_treat_control) %>%
  pivot_longer(!condition2) %>%
  pivot_wider(names_from = condition2, values_from = value) %>%
  rename(Control = "0", Treatment = "1")

#write.csv(df_summary_stat, "summary_stat.csv", row = T)

table <- df_summary_stat %>%
  kbl(format = "latex",
      digits = 2,
      align = 'c')
