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
                  acc                =((exp_value_chosen - exp_value_unchosen)>0)*1
)

contrasts(df$group)
contrasts(df$reward)
contrasts(df$reward_twoback)
contrasts(df$reward_oneback)


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

#in case we want to use Stan
df = df %>% mutate(offer1 = offer_left_image,
                   offer2 = offer_right_image,
                   choice = chosen,
                   selected_offer = ifelse(choice==offer1,0,1),
                   first_trial_in_block = (block!=lag(block,default=0))*1)

"left with 9444 trials for the whole sample"

df["reward"] = as.integer(unlist(df["reward"])) - 1
df["offer1"] = as.integer(unlist(df["offer1"]))
df["offer2"] = as.integer(unlist(df["offer2"]))
df["choice"] = as.integer(unlist(df["choice"]))
df["selected_offer"] = as.integer(unlist(df["selected_offer"]))

df$delay = df$delay_condition
df = df %>% mutate(delay_condition = ifelse(delay == 1, 0, 1))


save(df, file='./data/stanmodel_baseline_fourarms/df.rdata')
write.csv(df, file='./data/stanmodel_baseline_fourarms/df.csv')

