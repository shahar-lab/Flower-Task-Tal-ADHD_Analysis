rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)

#### housekeeping -------------
df$acc<-df$acc*100
contrasts(df$delay_condition)  = c(-1,1)
contrasts(df$group)  = c(-1,1)
contrasts(df$delay_condition)
contrasts(df$group)


#### acc ~ trial -------------------

#specify the model
f_model = acc~ group*delay_condition + (delay_condition | subject)


#set priors
get_prior(f_model,data=df) 

myprior = c(
  prior(normal(50, 25), class = Intercept),
  prior(normal(0, 25),  class = b        , coef = 'delay_condition1'),
  prior(normal(0, 25),  class = b        , coef = 'group1'),
  prior(normal(0, 25),  class = b        , coef = 'group1:delay_condition1'),
  prior(student_t(10,0,50), class = sigma )
)


#sample
model<-brm(f_model, 
           data   = df ,
           warmup = 2000,
           iter   = 3000,    
           cores  = 4,
           chains = 4,
           prior = myprior,
           backend='cmdstan')

library(bayestestR)
describe_posterior(model, rope_range = c(-0.1,+0.1))
conditional_effects(model)
save(model,file='./data/acc~groupXcondition_effectcoding.rdata')

