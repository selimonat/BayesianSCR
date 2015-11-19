generate_scr_data = function(nT=1000,
                             nOnsets = 10,
                            onsetsA = sample.int(nT,nOnsets),
                            onsetsB = sample.int(nT,nOnsets),
                            latA = 10+rnorm(1,0,1),
                            latB = 15+rnorm(1,0,1),
                            ampA = 6+rnorm(1,0,1),
                            ampB = 3+rnorm(1,0,1),
                            tau1 = 4+rnorm(1,0,1),
                            tau2 = 2+rnorm(1,0,1),
                            sigmaSCR = 0.05+rnorm(1,0,.1),
                            debug=F){
#  source('scr_model.R')
  
  
  while(sigmaSCR <= 0){
    sigmaSCR = 0.05+rnorm(1,0,.1)
  }
  while(tau1 <= 0){
    tau1 = 4+rnorm(1,0,1)
  }
  while(tau2 >= tau1 | tau2<=0){
    tau2 = 2+rnorm(1,0,1)
  }
  scr_trial = scr_model(x = 1:nT,
                        onsets = c(onsetsA,onsetsB),
                        amp = rnorm(length(onsetsA)+length(onsetsB),c(rep(ampA,length(onsetsA)),rep(ampB,length(onsetsB))),0),
                        latency = rnorm(length(onsetsA)+length(onsetsB),c(rep(latA,length(onsetsA)),rep(latB,length(onsetsB))),0),
                        tau1 = tau1,
                        tau2 = tau2)
  return(scr_trial+rnorm(nT,0,sigmaSCR))
}