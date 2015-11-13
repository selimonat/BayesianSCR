data {
  
  int<lower=0> nsubject;
  int ntrial[nsubject];#number of stimuli per subject
  int ntime_per_subject[nsubject]; # number of timepoints per subject
  int ncondition; # number of conditions (same for all subjects)
  real scr[sum(ntime_per_subject)]; # The SCR Data, concatenated for each subject
  vector[sum(ntime_per_subject)] x_per_subject; # A simple index from 1:ntime, contactenated for all subjects
  vector[sum(ntrial)] onset; # the onsets, concatenated for each subject (we know which is which due to ntrial)
  int<lower=1,upper=ncondition> condition[sum(ntrial)]; # which Condition [a/b]?, again concatenated
  
  
}

transformed data{  
}

parameters {
  
 #real <lower=0> m_latency[ncondition];
 #real <lower=0> s_latency[ncondition];
  vector <lower=0>[ncondition] latency[nsubject];
  
  #real <lower=0> m_amp[ncondition];
  #real <lower=0> s_amp[ncondition];
  vector <lower=0>[ncondition] amp[nsubject];
  
  #real <lower=0> m_tau1;
  #real <lower=0> s_tau1;
  real <lower=0> tau1[nsubject];
  
  #real <lower=0> m_tau2;
  #real <lower=0> s_tau2;
  real <lower=0> tau2[nsubject];
  
  #real <lower=0> m_scr_sigma;
  #real <lower=0> s_scr_sigma;
  real <lower=0> scr_sigma[nsubject];
  
}

transformed parameters {
  real maxx;
  real maxamp;
  
  real scr_hat[sum(ntime_per_subject)];
  
  
  scr_hat      <- rep_array(0,sum(ntime_per_subject));
  { #curly brackets allow for local integer usage
    
    int posTime;
    int posTrial;
    real c;
    int posPositive[1];
    posTime <- 1;
    posTrial<- 1;
    for(sub in 1:nsubject){
      vector[ntime_per_subject[sub]] xx;
      
      
      maxx   <- tau1[sub] * tau2[sub] * log(tau1[sub]/tau2[sub]) / (tau1[sub] - tau2[sub]);
      maxamp <- fabs(exp(-maxx/tau2[sub]) - exp(-maxx/tau1[sub]));
      
      for(tr in 1:ntrial[sub]){
        #print("one trial/onset in subject:",sub," - segment:",tr,"  ");
        //print("posTime:",posTime," ntime_per_subject:",ntime_per_subject);
        xx <- segment(x_per_subject,posTime,ntime_per_subject[sub]) - onset[posTrial+tr-1] -latency[sub][condition[tr]];
        
        c          <- amp[sub][condition[tr]]/maxamp;
        posPositive <-head(sort_indices_asc(xx .* xx),1);
        
        for(t in posPositive[1]:min(posPositive[1]+40,ntime_per_subject[sub])){
          //             if(tr == 1 || tr ==ntrial[sub]){
            //              print("t:",t);
            //            print("scr_hat:",scr_hat);
            //            
              //           }
          scr_hat[posTime+t-1]   <- scr_hat[posTime+t-1] + c * (exp(-xx[t]/tau1[sub]) - exp(-xx[t]/(tau2[sub])));
        }
        
      } #end for
      posTime <- posTime + ntime_per_subject[sub];
      posTrial <- posTrial + ntrial[sub];
    }# end for subject
  }# end curly
  
}# end transform

model {
  int posTimeModel;
//   m_amp[1] ~ normal(1,.1);
//   m_amp[2] ~ normal(1,.1);
//   s_amp[1] ~ normal(.1,.01);
//   s_amp[2] ~ normal(.1,.01);
//   
//   m_latency[1] ~ normal(10,.1);
//   m_latency[2] ~ normal(10,.1);
//   s_latency[1] ~ normal(1,.1);
//   s_latency[2] ~ normal(1,.1);
//   
//   m_tau1 ~ normal(4,.1);
//   s_tau1 ~ normal(.1,.01);
//   m_tau2 ~ normal(3,.1);
//   s_tau2 ~ normal(.1,.01);
//   
//   m_scr_sigma ~ normal(0.05,0.01);
//   s_scr_sigma ~ normal(0.1,0.01);
  
  
  posTimeModel <- 1;
  for (sub in 1:nsubject){
    
    // Hierarchical relation definition
//     for(c in 1:ncondition){
//       amp[sub][c]            ~ normal(m_amp[c],s_amp[c]);
//       latency[sub][c]        ~ normal(m_latency[c],s_latency[c]);
//     }
//     tau1[sub]               ~ normal(m_tau1,s_tau1);
//     tau2[sub]               ~ normal(m_tau2,s_tau2);
//     scr_sigma[sub]          ~ normal(m_scr_sigma,s_scr_sigma);
    
    // Model Definition
    segment(scr,posTimeModel,ntime_per_subject[sub]) ~ normal(segment(scr_hat,posTimeModel,ntime_per_subject[sub]),scr_sigma[sub]);
    posTimeModel <- posTimeModel+ntime_per_subject[sub];
  }
  
  
  
}

generated quantities {
  
  
}