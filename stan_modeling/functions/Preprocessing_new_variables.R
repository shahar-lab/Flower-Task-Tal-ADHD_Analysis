rm(list=ls())
source('./functions/my_starter.R')

library(bayestestR)
library(ggplot2)
library(ggpubr)
library(posterior)
library(RLR)
library(dplyr)
library(readr)

mydatatype=set_datatype()
path=set_workingmodel()

load(paste0(path$data,'/empirical_data_Qvalues_param.Rdata'))

self_report_only_scores <- read_csv("data/self-report_only_scores.csv")

df_data_Qval_parameters$subject = as.numeric(df_data_Qval_parameters$subject)

df = left_join(x = df_data_Qval_parameters, y = self_report_only_scores, by = c("subject" = "subject"))

df = df %>% mutate(PE_oneback = lag(PE, n=1),
                   PE_twoback = lag(PE, n=2),
                   PE_threeback = lag(PE, n=3),
                   PE_oneforward = lead(PE, n=1),
                   PE_twoforward = lead(PE, n=2),
                   PE_threeforward = lead(PE, n=3),
                   PE_abs = abs(PE),
                   PE_oneback_abs = abs(lag(PE)),
                   PE_twoback_abs = abs(lag(PE, n=2)),
                   PE_threeback_abs = abs(lag(PE, n=3)),
                   PE_oneforward_abs = abs(lead(PE, n=1)),
                   PE_twoforward_abs = abs(lead(PE, n=2)),
                   PE_threeforward_abs = abs(lead(PE, n=3)),
                   PE_sign = ifelse(PE > 0, 1, ifelse(PE < 0, -1, 0)),
                   PE_sign_oneback = lag(PE_sign, n=1),
                   PE_sign_twoback = lag(PE_sign, n=2),
                   PE_sign_threeback = lag(PE_sign, n=3),
                   PE_sign_oneforward = lead(PE_sign, n=1),
                   PE_sign_twoforward = lead(PE_sign, n=2),
                   PE_sign_threeforward = lead(PE_sign, n=3),
                   reward_coding = ifelse(reward == 0, -1, 1),
                   reward_coding_oneback = lag(reward_coding, n=1),
                   reward_coding_twoback = lag(reward_coding, n=2),
                   reward_coding_threeback = lag(reward_coding, n=3))

save(df, file=paste0(path$data,'/df_with_params.Rdata'))
write.csv(df, file=paste0(path$data,'/df_with_params.csv'), row.names=FALSE)
