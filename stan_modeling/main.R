rm(list=ls())
source('./functions/my_starter.R')
path=set_workingmodel()


#####simulate data--------------------

cfg = list(Nsubjects        = 60,
           Nblocks          = 8,
           Ntrials_perblock = 25,
           Narms            = 4,  #number of arms in the task 
           Nraffle          = 2,  #number of arms offered for selection each trial
           delay = c(1,7),
           rndwlk           = read.csv('./functions/rndwlk.csv',header=F),
           group            = read.csv('./functions/group.csv',header=F)
           )

simulate_parameters(path,cfg,plotme=T)

simulate_artifical_data(path,cfg)

# if empirical data
load('./data/stanmodel_baseline_fourarms/df.rdata')

df = df %>% filter(group == 'ADHD' & delay_condition == 1)

save(df,file = './data/stanmodel_baseline_fourarms/df_adhd_long_data.rdata')

mydatatype = 'df_adhd_long'

#load(paste0(path$data,'/empirical_data.Rdata'))
#mydatatype=set_datatype()

simulate_convert_to_standata(mydatatype,path,cfg,
                             var_toinclude  = c(
                              'first_trial_in_block',
                              'trial',
                              'offer1',
                              'offer2',
                              'choice',
                              'unchosen',
                              'selected_offer',
                              'reward',
                              'delay',
                              'delay_condition',
                              'group')
)

#####sample posterior--------------------

modelfit_compile(path)

modelfit_mcmc(path,mymcmc = list(
                datatype = mydatatype ,
                samples  =  1000,
                warmup  = 2000,
                chains  = 4,
                cores   = 4)
)

#examine_mcmc(path) #needs debugging

examine_population_parameters_recovery(path,mydatatype)

examine_individual_parameters_recovery(path,mydatatype)


####examine model
#load parameters
fit   = readRDS(paste0(path$data,'/',mydatatype,'_modelfit_empirical.rds'))
Qdiff = fit$draws(variables ='Qdiff_external',format='draws_matrix')
Qval1 = fit$draws(variables ='Qval1_external',format='draws_matrix')
Qval2 = fit$draws(variables ='Qval2_external',format='draws_matrix')
Qval3 = fit$draws(variables ='Qval3_external',format='draws_matrix')
Qval4 = fit$draws(variables ='Qval4_external',format='draws_matrix')

PE    = fit$draws(variables ='PE_external',format='draws_matrix')

####Documentation:------------------
#
#simulate_parameters:
#This function generate artificial model parameters in an hierarchical structure
#based on the definition in "_artificial_parameters.r" file that you need
#to place in the specific model folder.
#It then generate a 'model_parameters.Rdata' in the model data folder
#
#simulate_artifical_data:
#This function generates artificial data based on 'model_parameters.Rdata'
#and creates a df.rdata with artifical behavior in the model data folder
#
#simulate_convert_to_standata:
#converts the dataframe to matrix format and handles missing data with padding
#
#modelfit_compile:
#compile and save stan model (so you wont have to redo this everytime you re-run the model with different parameters)
#
#
#examine_parameters_recovery:
#This function plot recovered parameters against the true parameters
#use debug() and undebug() to find problems in these functions

