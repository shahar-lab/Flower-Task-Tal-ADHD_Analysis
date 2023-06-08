rm(list=ls())
load('./data/df.rdata')
library(dplyr)
library(brms)
library(emmeans)

#### housekeeping -------------

load('./data/acc~groupXcondition_dummycoding.rdata')

emmeans(model,~ group)
emmeans(model,~ group) |> pairs()


#> control group had a 55.3% accuracy (52.9-57.4) and the adhd group had 




load('./data/acc~groupXcondition_effectcoding.rdata')

emmeans(model,~ group)
emmeans(model,~ group) |> pairs()


#> control group had a 55.3% accuracy (52.9-57.4) and the adhd group had 
