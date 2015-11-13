generate_scr_data = function(nT=1000,
                             nOnsets = 10,
                            onsetsA = sample.int(nT,nOnsets),
                            onsetsB = sample.int(nT,nOnsets),
                            latA = 10+rnorm(1,0,1),
                            latB = 10+rnorm(1,0,1),
                            ampA = 1+rnorm(1,0,.1),
                            ampB = 1+rnorm(1,0,.1),
                            tau1 = 4+rnorm(1,0,.1),
                            tau2 = 3+rnorm(1,0,.1),
                            sigmaSCR = 0.05+rnorm(1,0,.1),
                            debug=F){
#  source('scr_model.R')
  while(sigmaSCR <= 0){
    sigmaSCR = 0.05+rnorm(1,0,.1)
  }
  scr_trial = scr_model(1:nT,c(onsetsA,onsetsB),c(rep(ampA,length(onsetsA)),rep(ampB,length(onsetsB))),c(rep(latA,length(onsetsA)),rep(latB,length(onsetsA))),tau1,tau2)
  return(scr_trial+rnorm(nT,0,sigmaSCR))
}