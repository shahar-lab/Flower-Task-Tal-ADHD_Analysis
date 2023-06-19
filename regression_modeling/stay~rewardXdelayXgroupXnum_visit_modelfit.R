rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)

#### housekeeping  -------------
df=df|>filter(trial>1)
df$delay_condition<-as.factor(df$delay_condition)
contrasts(df$delay_condition)
contrasts(df$group)
contrasts(df$reward_oneback)

df$stay = df$stay*100

####model sampling --------
#specify the model
f_model = stay~ reward_oneback*delay_condition*group*cum_visit_chosen + (reward_oneback*delay_condition*cum_visit_chosen | subject)


#set priors
get_prior(f_model,data=df) 

myprior = c(
  prior(normal(50, 25), class = Intercept),
  prior(normal(0, 25),  class = b        , coef = 'delay_condition7'),
  prior(normal(0, 25),  class = b        , coef = 'groupADHD'),
  prior(normal(0, 25),  class = b        , coef = 'reward_onebackrewarded'),
  prior(normal(0, 15),  class = b        , coef = 'delay_condition7:groupADHD'),
  prior(normal(0, 15),  class = b        , coef = 'reward_onebackrewarded:delay_condition7'),
  prior(normal(0, 15),  class = b        , coef = 'reward_onebackrewarded:groupADHD'),
  prior(normal(0, 15),  class = b        , coef = 'reward_onebackrewarded:delay_condition7:groupADHD'),
  prior(student_t(10,0,50), class = sigma )
)


#sample
model<-brm(f_model, 
           data   = df ,
           warmup = 1000,
           iter   = 1500,    
           cores  = 4,
           chains = 4,
           #prior = myprior,
           backend='cmdstan')


library(bayestestR)
describe_posterior(model, rope_range = c(-0.1,+0.1))
conditional_effects(model)
#save(model,file='./data/stay~reward_onebackXgroupXcondition.rdata')

