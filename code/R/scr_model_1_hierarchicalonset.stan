data {
  int ntrial;#number of stimuli
  int ntime; # number of timepoints
  int ncondition; # number of conditions
  real scr[ntime];
  vector[ntime] x;
  real<lower=0,upper=ntime> onset[ntrial]; # 
  int<lower=1,upper=ncondition> condition[ntrial]; # which Condition [a/b]?
}

transformed data{  
}

parameters {
  vector <lower=0>[ncondition] latency;
  vector <lower=0>[ncondition] amp;
  vector <lower=0>[ncondition] amp_sigma;
  matrix <lower=0>[ncondition,ntrial] amp_per_onset;
  real <lower=0> tau1;
  real <lower=0,upper=tau1> tau2;
  real <lower=0> scr_sigma;
}

transformed parameters {
}
model {
  real maxx;
  real maxamp;
  vector[ntime] scr_hat;
  
  for(c in 1:ncondition){
    for(tr in 1:ntrial){
      amp_per_onset[c,tr] ~ normal(amp[c],amp_sigma[c]);
    }
  }
  { #curly brackets allow for local integer usage
    vector[ntime] xx;
    int posPositive[1];
    
    real c;
    
    maxx   <- tau1 * tau2 * log(tau1/tau2) / (tau1 - tau2);
    maxamp <- fabs(exp(-maxx/tau2) - exp(-maxx/tau1));
    
    scr_hat      <- rep_vector(0,ntime);
    for(tr in 1:ntrial){
      
      xx <- x - onset[tr]-latency[condition[tr]];
      posPositive <-head(sort_indices_asc(xx .* xx),1);
      
      c          <- amp_per_onset[condition[tr],tr]/maxamp;
      
      for(t in posPositive[1]:min(posPositive[1]+100,ntime)){
        
        scr_hat[t]   <- scr_hat[t] + c * (exp(-xx[t]/tau1) - exp(-xx[t]/(tau2)));
        
      }# time loop
    } # trial loop
  }#curly
  
  
  scr ~ normal(scr_hat,scr_sigma);
}

generated quantities {
  
  
}