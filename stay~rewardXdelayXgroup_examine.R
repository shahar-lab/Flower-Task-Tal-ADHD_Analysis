rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)

#### housekeeping -------------
df$delay_condition<-as.factor(df$delay_condition)
df$acc<-df$acc*100
df$trial_centered <- scale(df$trial,center = T,scale = F)
#note - we didnt exclude the first trial since this is acc

load('./data/stay~reward_onebackXgroupXcondition.rdata')

library(ggplot2)
source('./mytheme.r')

plot(conditional_effects(model,effects = "reward_oneback:group", 
                    conditions = make_conditions(model, "delay_condition")),plot=F)[[1]]+mytheme


em=emmeans::emmeans(model,~reward_oneback:delay_condition:group)
cont=  emmeans::contrast(em, list('control'=c(1,-1, -1,1,0,0,0,0),
                                  'adhd'=c(0,0, 0,0,1,-1,-1,1),
                                  'control_short'=c(-1,1, 0,0,  0,0,  0,0),
                                  'control_long' =c(0,0, -1,1,  0,0,  0,0),
                                  'adhd_short'   =c(0,0,  0,0, -1,1,  0,0),
                                   'adhd_long'   =c(0,0,  0,0,  0,0, -1,1)))


plot(emmeans::regrid(cont), CIs=T,horizontal = FALSE, colors = "orange")+
  theme_bw()+xlab('P(parameter|data)')+ylab('')+xlim(-2,2)+
  geom_vline(xintercept = 0,linetype="dashed",color = "gray", size=1)+
  theme(text = element_text(size = 14))
