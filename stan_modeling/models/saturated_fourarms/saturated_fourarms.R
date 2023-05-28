#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation

  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  group              = cfg$group[1,subject]
  rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  delay              = cfg$delay
  Qval               = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  df                 = data.frame()
  
  
  
for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0.5,Narms)))
  current_delay = delay[1 + block%%2]
  delay_condition = ifelse(current_delay == 1, 0, 1)
  
  #set parameters
  alpha = plogis(parameters['alpha0']+
                  parameters['alpha1']*group+
                  parameters['alpha2']*delay_condition+
                  parameters['alpha3']*group*delay_condition)
  
  beta  = parameters['beta0']+
            parameters['beta1']*group+
            parameters['beta2']*delay_condition+
            parameters['beta3']*group*delay_condition
  
  for (trial in 1:Ntrials_perblock){

    #computer offer
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #players choice
    p         = exp(beta*Qval[raffle]) / sum(exp(beta*Qval[raffle]))
    choice    = sample(raffle,1,prob=p)
    unchosen  = raffle[choice!=raffle]
    
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[choice,trial],expvalues[choice,trial]))
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            choice               = choice,
            selected_offer       = (choice==raffle[2])*1,
            unchosen             = unchosen,
            offer1               = raffle[1],
            offer2               = raffle[2],
            delay                = current_delay,
            delay_condition      = delay_condition,
            group                = group,
            expval_ch            = expvalues[choice,trial],
            expval_unch          = expvalues[raffle[choice!=raffle],trial],
            reward               = reward,
            alpha = alpha,
            beta=beta
            )
      
      dfnew=cbind(dfnew,Qval)
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
  }
}     
  return (df)
}