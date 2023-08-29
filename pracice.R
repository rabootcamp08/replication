library(tidyverse)
library(haven)
df_raw <- read_dta("Bursztyn_etal2020AER_rep/data/raw_data/01_main_exp_raw1.dta")
df_clean <- read_dta("Bursztyn_etal2020AER_rep/data/clean_data/01_main_exp_clean.dta")

summary(df_raw)
colnames(df_raw)
colnames(df_clean)
