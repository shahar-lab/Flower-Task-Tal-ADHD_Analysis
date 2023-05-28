rm(list=ls())

source('./functions/my_starter.R')

#mydatatype=set_datatype()
mydatatype = 'df_adhd_long'
path=set_workingmodel()


fit   = readRDS(paste0(path$data,'/',mydatatype,'_modelfit_empirical.rds'))

Qdiff = fit$draws(variables ='Qdiff_external',format='draws_matrix')
Qval1 = fit$draws(variables ='Qval1_external',format='draws_matrix')
Qval2 = fit$draws(variables ='Qval2_external',format='draws_matrix')
Qval3 = fit$draws(variables ='Qval3_external',format='draws_matrix')
Qval4 = fit$draws(variables ='Qval4_external',format='draws_matrix')

PE    = fit$draws(variables ='PE_external',format='draws_matrix')

#prepossessing Qval1
df = data.frame(Qval1 = apply(Qval1,2,mean))
st = row.names(df)[1]
index_trial = unlist(gregexpr("\\[", st)) + 1
df = df %>% mutate(trial_subject = substring(row.names(df),index_trial,nchar(row.names(df))-1))

df_Qval1 = df %>% mutate(subject_Qval1 = substring(trial_subject,unlist(gregexpr(',', trial_subject)) + 1,),
                                      trial_Qval1 = substring(trial_subject,1,unlist(gregexpr(',', trial_subject))-1))

#prepossessing Qval2
df = data.frame(Qval2 = apply(Qval2,2,mean))
st = row.names(df)[1]
index_trial = unlist(gregexpr("\\[", st)) + 1
df = df %>% mutate(trial_subject = substring(row.names(df),index_trial,nchar(row.names(df))-1))

df_Qval2 = df %>% mutate(subject_Qval2 = substring(trial_subject,unlist(gregexpr(',', trial_subject)) + 1,),
                         trial_Qval2 = substring(trial_subject,1,unlist(gregexpr(',', trial_subject))-1))


#prepossessing Qval3
df = data.frame(Qval3 = apply(Qval3,2,mean))
st = row.names(df)[1]
index_trial = unlist(gregexpr("\\[", st)) + 1
df = df %>% mutate(trial_subject = substring(row.names(df),index_trial,nchar(row.names(df))-1))

df_Qval3 = df %>% mutate(subject_Qval3 = substring(trial_subject,unlist(gregexpr(',', trial_subject)) + 1,),
                         trial_Qval3 = substring(trial_subject,1,unlist(gregexpr(',', trial_subject))-1))


#prepossessing Qval4
df = data.frame(Qval4 = apply(Qval4,2,mean))
st = row.names(df)[1]
index_trial = unlist(gregexpr("\\[", st)) + 1
df = df %>% mutate(trial_subject = substring(row.names(df),index_trial,nchar(row.names(df))-1))

df_Qval4 = df %>% mutate(subject_Qval4 = substring(trial_subject,unlist(gregexpr(',', trial_subject)) + 1,),
                         trial_Qval4 = substring(trial_subject,1,unlist(gregexpr(',', trial_subject))-1))


#prepossessing Qval_diff
df = data.frame(Qdiff = apply(Qdiff,2,mean))
st = row.names(df)[1]
index_trial = unlist(gregexpr("\\[", st)) + 1
df = df %>% mutate(trial_subject = substring(row.names(df),index_trial,nchar(row.names(df))-1))

df_Qdiff = df %>% mutate(subject_Qdiff = substring(trial_subject,unlist(gregexpr(',', trial_subject)) + 1,),
                         trial_Qdiff = substring(trial_subject,1,unlist(gregexpr(',', trial_subject))-1))


#prepossessing PE
df = data.frame(PE = apply(PE,2,mean))
st = row.names(df)[1]
index_trial = unlist(gregexpr("\\[", st)) + 1
df = df %>% mutate(trial_subject = substring(row.names(df),index_trial,nchar(row.names(df))-1))

df_PE = df %>% mutate(subject_PE = substring(trial_subject,unlist(gregexpr(',', trial_subject)) + 1,),
                         trial_PE = substring(trial_subject,1,unlist(gregexpr(',', trial_subject))-1))

#### Join the dfs ####
df_Qval = left_join(x = df_Qval1, 
                    y = df_Qval2, 
                    by = "trial_subject")

df_Qval = left_join(x = df_Qval, 
                    y = df_Qval3,
                    by = "trial_subject")

df_Qval = left_join(x = df_Qval, 
                    y = df_Qval4,
                    by = "trial_subject")

df_Qval = left_join(x = df_Qval, 
                    y = df_Qdiff,
                    by = "trial_subject")

df_Qval = left_join(x = df_Qval, 
                    y = df_PE,
                    by = "trial_subject")

df_Qval = na.omit(df_Qval)

df_Qvall = df_Qval %>% mutate(subject = subject_Qval1, trial = trial_Qval1) %>% 
  select(subject, trial,Qval1, Qval1, Qval2, Qval3, Qval4, Qdiff, PE)


#save
if(mydatatype=='artificial'){
  save(df_Qvall,file=paste0(path$data,'/artificial_Q_values.Rdata'))
} else {
    save(df_Qvall,file=paste0(path$data,'/',mydatatype,'_empirical_Q_values.Rdata'))
  }

load(paste0(path$data,'/',mydatatype,'_data.Rdata'))


df_empirical_Qvalues = cbind(df, df_Qval)
df_data_Qvalues = df_empirical_Qvalues %>% select(-c(trial_subject, subject_Qval1, trial_Qval1,
                                                          subject_Qval2, trial_Qval2,
                                                          subject_Qval3, trial_Qval3,
                                                          subject_Qval4, trial_Qval4,
                                                          subject_Qdiff, trial_Qdiff,
                                                          subject_PE, trial_PE))

#save
if (mydatatype=='artificial'){
  save(df_data_Qvalues,file=paste0(path$data,'/artificial_data_Qvalues.Rdata'))
  }else {save(df_data_Qvalues,file=paste0(path$data,'/',mydatatype,'_empirical_data_Qvalues.Rdata'))}

