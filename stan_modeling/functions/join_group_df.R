rm(list=ls())
source('./functions/my_starter.R')

library(bayestestR)
library(ggplot2)
library(ggpubr)
library(posterior)
library(RLR)
library(dplyr)

mydatatype = 'df_control_long'
path=set_workingmodel()

load(paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues_param.Rdata'))
plot(df_data_Qval_parameters$alpha)
plot(df_data_Qval_parameters$beta)
plot(df_data_Qval_parameters$PE)
df_control_long = df_data_Qval_parameters


mydatatype = 'df_control_short'

load(paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues_param.Rdata'))
plot(df_data_Qval_parameters$alpha)
plot(df_data_Qval_parameters$beta)
plot(df_data_Qval_parameters$PE)
df_control_short = df_data_Qval_parameters


mydatatype = 'df_adhd_long'

load(paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues_param.Rdata'))
plot(df_data_Qval_parameters$alpha)
plot(df_data_Qval_parameters$beta)
plot(df_data_Qval_parameters$PE)
df_adhd_long = df_data_Qval_parameters


mydatatype = 'df_adhd_short'

load(paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues_param.Rdata'))
plot(df_data_Qval_parameters$alpha)
plot(df_data_Qval_parameters$beta)
plot(df_data_Qval_parameters$PE)
df_adhd_short = df_data_Qval_parameters

t.test(df_adhd_long$alpha, df_adhd_short$alpha)
t.test(df_adhd_long$beta, df_adhd_short$beta)
t.test(df_control_long$alpha, df_adhd_long$alpha)
t.test(df_control_short$alpha, df_adhd_short$alpha)
t.test(df_control_long$beta, df_adhd_long$beta)
t.test(df_control_short$beta, df_adhd_short$beta)

df = rbind(df_control_long,df_control_short,df_adhd_long,df_adhd_short)

save(df,file=paste0(path$data,'/empirical_df.Rdata'))
write.csv(df, file=paste0(path$data,'/empirical_df.csv'), row.names=FALSE)    
