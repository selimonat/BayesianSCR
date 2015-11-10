generate_scr_data = function(nT=1000,
                             nOnsets = 10,
                            onsetsA = sample.int(nT,nOnsets),
                            onsetsB = sample.int(nT,nOnsets),
                            latA = 10+rnorm(1,0,1),
                            latB = 10+rnorm(1,0,1),
                            ampA = 1+rnorm(1,0,.1),
                            ampB = 1+runif(1,0,.1),
                            tau1 = 4,
                            tau2 = 3,
                            sigmaSCR = 0.05,
                            debug=F){
#  source('scr_model.R')
  
  scr_trial = scr_model(1:nT,c(onsetsA,onsetsB),c(rep(ampA,length(onsetsA)),rep(ampB,length(onsetsB))),c(rep(latA,length(onsetsA)),rep(latB,length(onsetsA))),tau1,tau2)
  return(scr_trial+rnorm(nT,0,sigmaSCR))
}