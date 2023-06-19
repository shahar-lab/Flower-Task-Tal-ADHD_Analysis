rm(list=ls())

df<- read.csv('./data/df_raw.csv')
library(dplyr)

df <-df |> filter(block_type==' test')
names(df)

df= df %>% mutate(subject            = factor(subject),
                  group              = factor(group,levels = c('control','ADHD'),labels =c("control","ADHD")),
                  stay               = (chosen == lag(chosen))*1, 
                  reward             = factor(reward, levels = c(0,1), labels = c('unrewarded','rewarded')),
                  reward_oneback     = lag(reward),
                  reward_twoback     = lag(reward,2),
                  reward_threeback   = lag(reward,3),
                  reoffer_chosen_oneback = (lag(chosen) == offer_left_image | lag(chosen) == offer_right_image ),
                  stay_key            = (choice_location == lag(choice_location))*1,
                  delta_exp_value    = abs(exp_value_chosen - exp_value_unchosen),
                  acc                = ((exp_value_chosen - exp_value_unchosen)>0)*1,
                  delay_condition    = factor(as.numeric(delay_condition),levels = c(1,7), labels = c('short','long')),
                  offer1 = offer_left_image,
                  offer2 = offer_right_image,
                  choice = chosen,
                  selected_offer = ifelse(choice==offer1,0,1)
) |>
  select(!offer_left_image,!offer_right_image,!chosen)

contrasts(df$group)
contrasts(df$reward)
contrasts(df$reward_twoback)
contrasts(df$reward_oneback)
contrasts(df$delay_condition)
unique(df$delay_condition)




#### add cumulative rewards and visits

cum_reward = matrix(0,nrow = nrow(df),ncol = 4)
cum_visits = matrix(0,nrow = nrow(df),ncol = 4)

chosen     = df$chosen
unchosen   = df$unchosen
reward     = df$reward

for ( i in 2:nrow(df)) {
  
  print(i)
  
  cum_reward[i,] = cum_reward[i-1,] 
  cum_visits[i,] = cum_visits[i-1,] 
  
  if (!is.na(chosen[i])){
    
    cum_reward[i,chosen[i]] = cum_reward[i,chosen[i]] + (as.numeric(reward[i])-1)
    cum_visits[i,chosen[i]] = cum_visits[i,chosen[i]] + 1
    
  }
  
  if (df$trial[i]==1){
    
    cum_reward[i,] = 0
    cum_visits[i,] = 0
    
  }
}
df$cum_reward_ch1 = cum_reward[,1]
df$cum_reward_ch2 = cum_reward[,2]
df$cum_reward_ch3 = cum_reward[,3]
df$cum_reward_ch4 = cum_reward[,4]

df$cum_visit_ch1  = cum_visits[,1]
df$cum_visit_ch2  = cum_visits[,2]
df$cum_visit_ch3  = cum_visits[,3]
df$cum_visit_ch4  = cum_visits[,4]


df$mean_reward_ch1 = ifelse(is.na(cum_reward[,1] / (cum_visits[,1])),0,(cum_reward[,1] / (cum_visits[,1])))
df$mean_reward_ch2 = ifelse(is.na(cum_reward[,2] / (cum_visits[,2])),0,(cum_reward[,2] / (cum_visits[,2])))
df$mean_reward_ch3 = ifelse(is.na(cum_reward[,3] / (cum_visits[,3])),0,(cum_reward[,3] / (cum_visits[,3])))
df$mean_reward_ch4 = ifelse(is.na(cum_reward[,4] / (cum_visits[,4])),0,(cum_reward[,4] / (cum_visits[,4])))

cum_reward_chosen = cum_reward_unchosen = 
  cum_visit_chosen  = cum_visit_unchosen  = 
  cum_visit_chosen  = cum_visit_unchosen  = 
  mean_reward_chosen = mean_reward_unchosen = 
  df$trial*0

for ( i in 1:nrow(df)) {
  
  cum_reward_chosen[i]   = cum_reward[i,chosen[i]]
  cum_reward_unchosen[i] = cum_reward[i,unchosen[i]]
  cum_visit_chosen[i]    = cum_visits[i,chosen[i]]
  cum_visit_unchosen[i]  = cum_visits[i,unchosen[i]]
  
  if(!is.na(cum_visit_chosen[i]) & cum_visit_chosen[i] > 0){
    mean_reward_chosen[i]    = cum_reward_chosen[i] / cum_visit_chosen[i]
  }
  if(!is.na(cum_visit_unchosen[i]) & cum_visit_unchosen[i] > 0){
    mean_reward_unchosen[i]  = cum_reward_unchosen[i] / cum_visit_unchosen[i]
  }
  
}
df$cum_reward_chosen   = cum_reward_chosen
df$cum_reward_unchosen = cum_reward_unchosen
df$cum_visit_chosen    = cum_visit_chosen
df$cum_visit_unchosen  = cum_visit_unchosen
df$cum_visit_chosen    = cum_visit_chosen
df$cum_visit_unchosen  = cum_visit_unchosen
df$mean_reward_chosen  = mean_reward_chosen
df$mean_reward_unchosen= mean_reward_unchosen


df$subj_acc = ifelse((df$mean_reward_chosen + df$mean_reward_unchosen)==0,NA,(df$mean_reward_chosen > df$mean_reward_unchosen)*1)


#### mark very long or very short RTs --------

df= df %>% mutate(abort = ( rt < 0.2 | rt > 5 | is.na(rt)))
df |> summarise(mean(abort))

"we discared trial with implusiable short (<200ms) or long (>5sec) reaction times (1.62% of the trials)"

#######omit subjects with more the 90% stay for key ----------------------

df%>%
  group_by(subject)%>%
  summarise(p.stay=mean(stay_key))%>%
  with(plot(p.stay,ylim=c(0,1)))

print('no subjects were ommited, all had pstay_key<0.9')

"we discared subjects that pressed the same ket more then 90% of the trial (no subjects were ommited)"


#######omit subjects with more then 25% trials aborted due to very long, very short RTs----------------------

p.abort=df%>%group_by(subject)%>%summarise(p.abort=mean(abort))%>%mutate(above_threshold=(p.abort>.25))
sum(p.abort$above_threshold)

# subj.list=p.abort$subject[p.abort$above_threshold==0]
# df=df%>%filter(subject%in%subj.list)

"we further aimed to ommit participants that had more the 25% trials discarded due to very fast/slow RTs yet no participants reached the criteria"

#######filter ----------------------

mean(df$abort)
print('we then ommited 1.62% of trials due to fast/slow rts or first trial in the block')

df=df%>%filter(abort==0)

sum(is.na(df)) 
"all the NAs are the results of lagging in the preprocessing. See df[!complete.cases(df), ] |> View()"

"left with 9444 trials for the whole sample"



####stan columns --------
df = df %>% mutate(first_trial_in_block = (block!=lag(block,default=0))*1)

save(df, file='./data/df.rdata')
write.csv(df, file='./data/df.csv')

