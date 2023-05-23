rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)

#### housekeeping -------------
df$delay_condition<-as.factor(df$delay_condition)
df$acc<-df$acc*100
df$trial_centered <- scale(df$trial,center = T,scale = F)
#note - we didnt exclude the first trial since this is acc

#### acc ~ trial -------------------

#specify the model
f_model = acc~ trial_centered + (trial_centered | subject)


#set priors
get_prior(f_model,data=df) 

myprior = c(
  prior(normal(50, 25), class = Intercept),
  prior(normal(0, 25),  class = b        , coef = 'trial_centered'),
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
save(model,file='./data/acc~trials.rdata')

