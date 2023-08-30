rm(list = ls())
#↑environmentのところを消す

library(tidyverse)
library(haven)
df_raw_01 <- read_dta("Bursztyn_etal2020AER_rep/data/raw_data/01_main_exp_raw1.dta")
df_clean <- read_dta("Bursztyn_etal2020AER_rep/data/clean_data/01_main_exp_clean.dta")

# rename beliefs of labor demand
df_raw_01|>
  dplyr::rename(labor_demand_guess=q45_1_p2)->df_raw_01

df_raw_01$labor_demand_guess<-ifelse(is.na(df_raw_01$labor_demand_guess),df_raw_01$q45_4_p2,
                                     ifelse(is.na(df_raw_01$labor_demand_guess),df_raw_01$q46_1_p2,
                                            ifelse(is.na(df_raw_01$labor_demand_guess),df_raw_01$q43_1_p2,df_raw_01$labor_demand_guess)))

# generating signup variable with wife's number
df_raw_01 <- df_raw_01|>
  dplyr::mutate(signed_up_number = signed_up,
                signed_up_number = case_when(
                    no_wife_number == 1 ~ 0,
                    TRUE ~ signed_up
                )) 
#	moving no condition people to control and including them
df_raw_01 <- df_raw_01|>
  dplyr::mutate(condition2 = condition,
                condition2 = case_when(
                  is.na(condition) ~ 0,
                  TRUE ~ condition
                )) 

#	(from now on, I will use condition2 (results are robust to using condition))
df_raw_01 <- df_raw_01|>
  dplyr::mutate(age = case_when(
    between(age, 18, 36)~age,
    TRUE ~ NA
  ))

df_raw_01 <- df_raw_01|>
  dplyr::mutate(college_deg = if_else(education>=6, 1, 0))

# 独身と死別は０。NAにするかも？
df_raw_01 <- df_raw_01|>
  dplyr::mutate(married = if_else(marital == 2, 1, 0))

df_raw_01 <- df_raw_01 |>
  dplyr::mutate(num_know_per = num_know/30,
                num_mfs_per = num_mutual_friends/30)

## wedge
df_raw_02 <- df_raw_01|>
  dplyr::mutate(outside_guess = outside_others + outside_self, 
                 )|>
  group_by(session)|>
  summarize( outside_objective = mean(outside_self)*30,
             semiseg_objective = mean(semiseg_self)*30,
             mwage_objective = mean(mwage_self)*30,.groups = "drop")  

df_raw_01<-left_join(df_raw_01,df_raw_02,"session")

df_raw_01<-df_raw_01|>
  dplyr::mutate(outside_guess = outside_others + outside_self, 
                outside_wedge= outside_guess-outside_objective,
                semiseg_guess = semiseg_others + semiseg_self, 
                semiseg_wedge= semiseg_guess-semiseg_objective,
                mwage_guess = mwage_others + mwage_self, 
                mwage_wedge= mwage_guess-mwage_objective,
                )

df_raw_01<-df_raw_01|>
  mutate(outside_wedge_pos = case_when(outside_wedge>0~1,TRUE~0),
         interaction = condition2*outside_wedge_pos)

outside_wedge_pos = (outside_wedge > 0)
interaction = condition2*outside_wedge_pos

df_final<-df_raw_01|>
  dplyr::select(-c(condition, marital, employed_ever, num_know, num_mutual_friends, q45_4_p2,
                     q46_1_p2, q43_1_p2, count, condition_txt, glowork_choice, second_order_total, 
                     signed_up, no_wife_number))

summary(df_raw)
colnames(df_raw)
colnames(df_clean)




