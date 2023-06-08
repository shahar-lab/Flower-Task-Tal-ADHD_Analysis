rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)

#### housekeeping -------------
df$delay_condition<-as.factor(df$delay_condition)
df$acc<-df$acc*100
df$trial_centered <- scale(df$trial,center = T,scale = F)
#note - we didnt exclude the first trial since this is acc

load('./data/acc~trials.rdata')

library(ggplot2)
source('./mytheme.r')
ggplot(df |> group_by(trial)|>summarise(acc=mean(acc)),aes(x=trial,y=acc))+geom_smooth(method = "loess")+geom_point()+
  ylim(45,65)+mytheme

#####regression ----
load('./data/acc~trials.rdata')
bayestestR::describe_posterior(model, rope_range = c(-0.1,+0.1))
bayestestR::p_direction(model, method = "direct", null = 0.5)

library(emmeans)
x= emtrends(model, var='trial_centered',at = list(trial_centered = c(min(df$trial_centered),max(df$trial_centered))))
summary(x)
x = posterior_epred(model,newdata = data.frame(trial_centered=-11.98751),)
