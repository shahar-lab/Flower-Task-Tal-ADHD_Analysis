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


fit   = readRDS(paste0(path$data,'/',mydatatype,'_modelfit_empirical.rds'))

pars <- as_draws_df(fit$draws())

# df_adhd_long - 24
# df_adhd_short - 24

# df_control_long - 24
# df_control_short - 24

# individual level parameters
pars_alpha = pars %>% select("alpha[1]":"alpha[24]")
pars_beta = pars %>% select("beta[1]":"beta[24]")

empirical_alpha = apply(pars_alpha, 2, mean)
empirical_beta = apply(pars_beta, 2, mean)

empirical_parameters = data.frame(alpha = empirical_alpha, beta = empirical_beta)

load(paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues.Rdata'))
subjects = df_data_Qvalues %>% select(subject) %>% distinct(subject) %>% mutate(number = row_number())

empirical_parameters = cbind.data.frame(empirical_parameters, subjects)

empirical_parameters = empirical_parameters %>% select(-number)


df_data_Qval_parameters = plyr::join(df_data_Qvalues, empirical_parameters, by = c("subject"), type = "left", match = "all")

#save
if (mydatatype=='artificial'){
  save(df_data_Qval_parameters,file=paste0(path$data,'/artificial_data_Qvalues.Rdata'))
} else {
  save(df_data_Qval_parameters,file=paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues_param.Rdata'))
}
# if (mydatatype=='empirical'){save(df_data_Qval_parameters,file=paste0(path$data,'/empirical_data_Qvalues_param.Rdata'))}

