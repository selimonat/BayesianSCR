data {
 
  
  int<lower=0> nsubject;
  int nonset[nsubject];#number of stimuli per subject
  int ntime_per_subject[nsubject]; # number of timepoints per subject
  int ncondition; # number of conditions (same for all subjects)
  vector[sum(ntime_per_subject)] scr; # The SCR Data, concatenated for each subject
  vector[sum(ntime_per_subject)] x_per_subject; # A simple index from 1:ntime, contactenated for all subjects
  vector                          [sum(nonset)] onset; # the onsets, concatenated for each subject (we know which is which due to nonset)
  int<lower=1,upper=ncondition>   condition[sum(nonset)]; # which Condition [a/b]?, again concatenated
  
  
}

transformed data{  
}

parameters {
  
  real <lower=0> m_latency[ncondition];
  real <lower=0> s_latency[ncondition];
  matrix <lower=0>[nsubject,ncondition] latency;
  
  real <lower=0> m_amp[ncondition];
  real <lower=0> s_amp[ncondition];
  matrix <lower=0>[nsubject,ncondition] amp;
  
  real <lower=0> m_tau1;
  real <lower=0> s_tau1;
  real <lower=0> tau1[nsubject];
  
  real <lower=0> m_tau2;
  real <lower=0> s_tau2;
  real <lower=0,upper=tau1[nsubject]> tau2[nsubject];
  
  real <lower=0> m_scr_sigma;
  real <lower=0> s_scr_sigma;
  real <lower=0> scr_sigma[nsubject];
  
}

transformed parameters {
  real maxx;
  real maxamp;
  
  vector[sum(ntime_per_subject)] scr_hat;
  
  
  scr_hat      <- rep_vector(0,sum(ntime_per_subject));
  { #curly brackets allow for local integer usage
  
  int posTime;
  int posonset;
  real c;
  int posPositive[1];
  posTime <- 1;
  posonset<- 1;
  for(sub in 1:nsubject){
    vector[ntime_per_subject[sub]] xx;
        
      
      maxx   <- tau1[sub] * tau2[sub] * log(tau1[sub]/tau2[sub]) / (tau1[sub] - tau2[sub]);
      maxamp <- fabs(exp(-maxx/tau2[sub]) - exp(-maxx/tau1[sub]));

      for(on in 1:nonset[sub]){
          xx <- segment(x_per_subject,posTime,ntime_per_subject[sub]) - onset[posonset+on-1] -latency[sub,condition[on]];
          c          <- amp[sub,condition[on]]/maxamp;
          
          posPositive <-head(sort_indices_asc(xx .* xx),1);
          for(t in posPositive[1]:min(posPositive[1]+40,ntime_per_subject[sub])){
          scr_hat[posTime+t-1]   <- scr_hat[posTime+t-1] + c * (exp(-xx[t]/tau1[sub]) - exp(-xx[t]/(tau2[sub])));
          
//               if(sub == nsubject && on ==nonset[sub] && t == min(posPositive[1]+40,ntime_per_subject[sub])){
//                 print("")
//                 # maxamp:",maxamp," amp:",amp[sub][condition[on]]," c:",c,
//                 print("lat:",latency[sub][condition[on]]," cond:",condition[on]," tau1:",tau1[sub]," tau2:",tau2[sub]);
//               }
          } #end t-loop
           
      } #end onset/onset loop
      posTime <- posTime + ntime_per_subject[sub];
      posonset <- posonset + nonset[sub];
    }# end for subject
  }# end curly
  
}# end transform

model {
  int posTimeModel;
    posTimeModel <- 1;
    for (sub in 1:nsubject){
       
        // Hierarchical relation definition
       for(c in 1:ncondition){
          amp[sub,c]            ~ normal(m_amp[c],s_amp[c]);
          latency[sub,c]        ~ normal(m_latency[c],s_latency[c]);
       }
       tau1[sub]               ~ normal(m_tau1,s_tau1);
       tau2[sub]               ~ normal(m_tau2,s_tau2);
       scr_sigma[sub]          ~ normal(m_scr_sigma,s_scr_sigma);
      
       // Model Definition
       
       # I checked the segment/ragged array & postimemodel and they are fine.
       segment(scr,posTimeModel,ntime_per_subject[sub]) ~ normal(segment(scr_hat,posTimeModel,ntime_per_subject[sub]),scr_sigma[sub]);
       posTimeModel <- posTimeModel+ntime_per_subject[sub];
    }
    
  
  
  
}

generated quantities {
  
  
}