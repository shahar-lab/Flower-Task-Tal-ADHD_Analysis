rm(list=ls())

files = dir('./raw_data')
df    = data.frame()

for (i in 1:length(files)){
  df_temp = read.csv(files[i])
  
  df_raw = rbind(df,df_temp)
  
}

write.csv()
  
  