scr_get_stan_data_simulation = function(cfg){
  source('scr_stan_data_simulation.R')
  source('generate_scr_data.R')
  source('scr_model.R')
  library(plyr)
  if(is.null(cfg$subject)){
    cfg$subject = -56:-1
  }
  simSCR = NULL
  init.fLoop = NULL
  for (s in cfg$subject){
    print(s)
    simSCRLoop = list(onset =seq(0,400,100))
    simSCRLoop$onset = simSCRLoop$onset+runif(length(simSCRLoop$onset),max = 10)
    genscr= (generate_scr_data(nT = 500,onsets = simSCRLoop$onset,latency=1,scr_sigma = 0.01,amp_sigma = 0.001))
    simSCRLoop$scr = genscr$scr
    #simSCRLoop$init.f = genscr$init.f
    if(length(cfg$subject)==1){
      simSCR = simSCRLoop
      init.fLoop = genscr$init.f
    }else{
      if(is.null(init.fLoop)){init.fLoop = genscr$init.f
      }else{
        concatlist = c('onsets','latency','amp','amp_sigma','amp_per_onset','tau1','tau2','scr_sigma')
        for(co in concatlist){
          init.fLoop[[co]] =c(init.fLoop[[co]],genscr$init.f[[co]])
        }
      }
      simSCR = rbind(simSCR,simSCRLoop)
    }
  }
  if(length(cfg$subject)>1){
    init.fLoop$latency = t(matrix(init.fLoop$latency,nrow=1))
    init.fLoop$amp     = t(matrix(init.fLoop$amp,nrow=1))
    init.fLoop$amp_sigma     = t(matrix(init.fLoop$amp_sigma,nrow=1))
    init.fLoop$amp_per_onset     = (matrix(init.fLoop$amp_per_onset,nrow=1))
  }else{
    init.fLoop$latency = as.array(init.fLoop$latency[1])
    init.fLoop$amp_per_onset     = t(init.fLoop$amp_per_onset)
  }
  #browser()
  dataSim = scr_stan_data_simulation(simSCR,cfg=cfg)
  data = list(data_stan = dataSim,
              init.f = init.fLoop)
  
  return(data)
  
  
  
}