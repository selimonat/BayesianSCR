scr_model = function(x,onsets,amp,latency,tau1,tau2){
  # %y = scr_model(x,onsets,param)
  # %
  # %x: time 
  # %onsets: stimulus onsets
  # %params: [amplitudes for each onsets tau1 dtau]; where tau2 = tau1 - dtau;
  
   maxx   = tau1 * tau2 * log(tau1/tau2) / (tau1 - tau2);
   maxamp = abs(exp(-maxx/tau2) - exp(-maxx/tau1));
  # c      =  amp/maxamp;
  
  y      = rep(0,length(x));
  for (e in 1:length(onsets)){;
    xx         = x - onsets[e]-latency[e];
    c          = amp[e]/maxamp;
    
    y[xx>=0]   = y[xx>=0] + c * (exp(-xx[xx>=0]/tau1) - exp(-xx[xx>=0]/(tau2)));
  }
  return(y)
}