data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    
  int<lower = 4> Narms;                                             
  int<lower = 2> Nraffle; 


  //Behavioral data:
  int<lower = 0> choice[Nsubjects,Ntrials];              
  int<lower = 0> reward[Nsubjects,Ntrials];              
  int<lower = 0> offer1[Nsubjects,Ntrials];              
  int<lower = 0> offer2[Nsubjects,Ntrials];              
  int<lower = 0> selected_offer[Nsubjects,Ntrials];      
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];
  int<lower = 0> group[Nsubjects, Ntrials];
  int<lower = 0> delay_condition[Nsubjects,Ntrials];

}


transformed data{
  int<lower = 1> Nparameters=8; 
  vector[Narms] Qvalue_initial; 
  Qvalue_initial = rep_vector(0.5, Narms);
}




parameters {
  //population level parameters 
  vector         [Nparameters] population_locations;      
  vector<lower=0>[Nparameters] population_scales;         
  
  //individuals level
  vector[Nsubjects] alpha0_random_effect;
  vector[Nsubjects] alpha1_random_effect;
  vector[Nsubjects] alpha2_random_effect;
  vector[Nsubjects] alpha3_random_effect;
  vector[Nsubjects] beta0_random_effect;
  vector[Nsubjects] beta1_random_effect;
  vector[Nsubjects] beta2_random_effect;
  vector[Nsubjects] beta3_random_effect;
}


transformed parameters {
  
  vector [Nsubjects] alpha0;
  vector [Nsubjects] alpha1;
  vector [Nsubjects] alpha2;
  vector [Nsubjects] alpha3;
  vector [Nsubjects] beta0;
  vector [Nsubjects] beta1;
  vector [Nsubjects] beta2;
  vector [Nsubjects] beta3;
  matrix [Ntrials,Nsubjects] p_ch_action;
  matrix [Ntrials,Nsubjects] Qdiff_external;
  matrix [Ntrials,Nsubjects] Qval1_external;
  matrix [Ntrials,Nsubjects] Qval2_external;
  matrix [Ntrials,Nsubjects] Qval3_external;
  matrix [Ntrials,Nsubjects] Qval4_external;
  matrix [Ntrials,Nsubjects] PE_external;



  //RL
  for (subject in 1:Nsubjects) {
    //internal variabels
    real   Qdiff;
    real   PE;
	  real   Qval[Narms];
	  real   alpha[Nsubjects, Ntrials];
	  real   beta[Nsubjects, Ntrials];
	  
	  alpha0[subject] = (population_locations[1]  + population_scales[1] * alpha0_random_effect[subject]);
	  alpha1[subject] = (population_locations[2]  + population_scales[2] * alpha1_random_effect[subject]);
	  alpha2[subject] = (population_locations[3]  + population_scales[3] * alpha2_random_effect[subject]);
	  alpha3[subject] = (population_locations[4]  + population_scales[4] * alpha3_random_effect[subject]);
    
    beta0[subject] = (population_locations[5]  + population_scales[5] * beta0_random_effect[subject]);
	  beta1[subject] = (population_locations[6]  + population_scales[6] * beta1_random_effect[subject]);
	  beta2[subject] = (population_locations[7]  + population_scales[7] * beta2_random_effect[subject]);
	  beta3[subject] = (population_locations[8]  + population_scales[8] * beta3_random_effect[subject]);
    
    //likelihood estimation
    for (trial in 1:Ntrials_per_subject[subject]){
      
      //reset Qvalues (first trial only)
    	if (first_trial_in_block[subject,trial] == 1) {
        Qval = rep_array(0.5, Narms);
    	}
    		
  		//set indvidual parameters
      alpha[subject,trial] = inv_logit(alpha0[subject] + alpha1[subject]*group[subject, trial] + alpha2[subject]*delay_condition[subject,trial] + alpha3[subject]*group[subject,trial]*delay_condition[subject,trial]);
      
      beta[subject,trial] = (beta0[subject] + beta1[subject]*group[subject,trial] + beta2[subject]*delay_condition[subject,trial] + beta3[subject]*group[subject,trial]*delay_condition[subject,trial]);
      
          
      //calculate probability for each action
      Qdiff        = Qval[offer2[subject,trial]]- Qval[offer1[subject,trial]];

      p_ch_action[trial,subject] = inv_logit(beta[subject,trial]*Qdiff);
      
      //update Qvalues
      PE  = reward[subject,trial]  - Qval[choice[subject,trial]];
      Qval[choice[subject,trial]] = Qval[choice[subject,trial]]+alpha[subject,trial]*PE;
      
      //appened to external variabels
      Qdiff_external[trial,subject] = Qdiff;
      Qval1_external[trial,subject] = Qval[1];
      Qval2_external[trial,subject] = Qval[2];
      Qval3_external[trial,subject] = Qval[3];
      Qval4_external[trial,subject] = Qval[4];
      PE_external[trial,subject]    = PE;
      
      
    }
 
  }

}


model {
  real beta[Nsubjects, Ntrials];
  // population level  
  population_locations  ~ normal(0,2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha0_random_effect ~ std_normal();
  alpha1_random_effect ~ std_normal();
  alpha2_random_effect ~ std_normal();
  alpha3_random_effect ~ std_normal();
  beta0_random_effect  ~ std_normal();
  beta1_random_effect  ~ std_normal();
  beta2_random_effect  ~ std_normal();
  beta3_random_effect  ~ std_normal();
  
  
  for (subject in 1:Nsubjects){
    for(trial in 1:Ntrials_per_subject[subject]){
      beta[subject, trial] = (beta0[subject] + beta1[subject]*group[subject,trial] + beta2[subject]*delay_condition[subject,trial] + beta3[subject]*group[subject,trial]*delay_condition[subject,trial]);
      target+= bernoulli_logit_lpmf(selected_offer[subject,trial] |  beta[subject, trial]* Qdiff_external[trial,subject]);
    }
  }
}
