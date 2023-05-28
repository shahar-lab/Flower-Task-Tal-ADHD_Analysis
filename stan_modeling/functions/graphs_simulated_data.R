rm(list=ls())
source('./functions/my_starter.R')

library(dplyr)
library(ggplot2)
library(effects)
library(brms)
library(rstan)
library(bayestestR)
library(cmdstanr)
library(readr)

mydatatype=set_datatype()
path=set_workingmodel()

load(paste0(path$data,'/artificial_data.Rdata'))

df = df %>% mutate(Qchosen = (`1`*(choice==1)*1)+ (`2`*(choice==2)*1) + (`3`*(choice==3)*1) +(`4`*(choice==4)*1))

ggplot(df %>% filter(subject < 6 & block == 7), aes(x=trial, y=Qchosen)) +
  geom_point() + geom_line()+ facet_grid(subject ~ .)
