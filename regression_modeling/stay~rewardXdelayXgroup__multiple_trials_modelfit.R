rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)

#### housekeeping  -------------
df=df|>filter(trial>3)
contrasts(df$delay_condition)
contrasts(df$group)
contrasts(df$reward_oneback)
contrasts(df$reward_twoback)
contrasts(df$reward_threeback)

df$stay = df$stay*100

df = 
  df |> select(subject,reward_oneback,reward_twoback,reward_threeback,delay_condition,group,stay)

df = na.omit(df)

####model sampling --------
#specify the model
f_model = stay~ reward_oneback*delay_condition*group + 
                reward_twoback*delay_condition*group +
                reward_threeback*delay_condition*group +
  
                (reward_oneback*delay_condition +
                 reward_twoback*delay_condition*group +
                 reward_threeback*delay_condition*group | subject)


#set priors
get_prior(f_model,data=na.omit(df) )

myprior = c(
  prior(normal(50, 25), class = Intercept),
  prior(normal(0, 25),  class = b        , coef = 'delay_conditionlong'),
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
           warmup = 2000,
           iter   = 3000,    
           cores  = 4,
           chains = 4,
           #prior = myprior,
           backend='cmdstan')


library(bayestestR)
describe_posterior(model, rope_range = c(-0.1,+0.1))
conditional_effects(model)
save(model,file='./data/stay~reward_onebackXgroupXcondition_multipletrials.rdata')


emmeans()


conditional_effects(model,effects = "delay_condition:reward_threeback", conditions = make_conditions(model, "group"))
