data {
  int ntrial;#number of stimuli
  int ntime; # number of timepoints
  int ncondition; # number of conditions
  real scr[ntime];
  vector[ntime] x;
  int<lower=1> onset[ntrial]; # 
  int<lower=1,upper=ncondition> condition[ntrial]; # which Condition [a/b]?
}

transformed data{  
}

parameters {
  
  #real <lower=0> m_latency[ncondition];
  #real <lower=0> s_latency[ncondition];
  vector <lower=0>[ncondition] latency;
  
  #real <lower=0> m_amp[ncondition];
  #real <lower=0> s_amp[ncondition];
  vector <lower=0>[ncondition] amp;
  
  #real <lower=0> m_tau1;
  #real <lower=0> s_tau1;
  real <lower=0> tau1;
  
  #real <lower=0> m_tau2;
  #real <lower=0> s_tau2;
  real <lower=0> tau2;
  
  #real <lower=0> m_scr_sigma;
  #real <lower=0> s_scr_sigma;
  real <lower=0> scr_sigma;
  
}

transformed parameters {
  real maxx;
  real maxamp;
  vector[ntime] scr_hat;
  
  
  { #curly brackets allow for local integer usage
    vector[ntime] xx;
    int posPositive[1];
    int windowSize;
  real c;
  
  maxx   <- tau1 * tau2 * log(tau1/tau2) / (tau1 - tau2);
  maxamp <- fabs(exp(-maxx/tau2) - exp(-maxx/tau1));
  
  scr_hat      <- rep_vector(0,ntime);
  for(tr in 1:ntrial){
      #xx        = x - onsets[e]-latency[e];
//       for (t in 1:ntime){
//         xx[t]         <- x[t] - onset[tr]- latency[condition[tr]];
//       }
      xx <- x - onset[tr]-latency[condition[tr]];
      posPositive <-head(sort_indices_asc(xx .* xx),1);
      
      c <- amp[condition[tr]]/maxamp;
      
      // segment(scr_hat,posPositive[1],ntime-posPositive[1])
      // segment(scr_hat,posPositive[1],ntime-posPositive[1])   <- segment(scr_hat,posPositive[1],ntime-posPositive[1]);
      #+ c * (exp(-segment(xx,posPositive,ntime-posPositive)/tau1) - exp(-segment(xx,posPositive,ntime-posPositive)/(tau2)));
      windowSize <- ntime;
      #for(t in posPositive[1]:min(ntime,(posPositive[1]+windowSize))){
        for(t in posPositive[1]:ntime){
        //segment(scr_hat,posPositive[1]+t-1,1)<-scr_hat[t] + c * (exp(-xx[t]/tau1) - exp(-xx[t]/(tau2)));
         scr_hat[t]   <- scr_hat[t] + c * (exp(-xx[t]/tau1) - exp(-xx[t]/(tau2)));
        
      }
  } 
  }
  
  
}

model {
  # Hyperpriors  (the real ones)
  
  #
  
    # parameter priors
    
#    Rinit[tr]          ~ normal(m_Rinit,s_Rinit);
    
    #for (t in 1:ntime){
      #scr[t] ~ normal(scr_hat[t],scr_sigma);
       scr ~ normal(scr_hat,scr_sigma);
    #}
  
  
  
}

generated quantities {
  
  
}