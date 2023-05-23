rm(list=ls())


#### merge files ----------
files  = dir('./data/raw_data')
df_raw = data.frame()

for (i in 1:length(files)){
  
  if (i ==1 ){
    df_raw = read.csv(paste0('./data/raw_data/',files[i]))
  } else {
    df_temp = read.csv(paste0('./data/raw_data/',files[i]),
                       header = T)  
    df_raw  = rbind(df_raw,df_temp)  
  }
  
}



####basic housekeeping-----------
library(dplyr)
names(df_raw)
df_raw = df_raw %>% mutate(subject            = factor(ShaharID),
                           group              = factor(group,levels = c(100,200),labels = c("control","ADHD")),
                           counterbalance     = factor(counterbalance,levels = c(0,1),labels = c("start_with_short_delay","start_with_long_delay")),
                           rt = RT) |>
                    select(-ShaharID,-RT)

write.csv(df_raw,file='./data/df_raw.csv',row.names = F)
  
  