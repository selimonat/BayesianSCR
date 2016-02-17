#!/usr/bin/Rscript
if (1==0){
  cfg_grid = list(nchains = 1,
                  gridOutputPath = file.path('./gridoutput'),
                  requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
                  job_name = 'scr_singlesub',
                  script_name = 'fit_data_model_1_hierarchicalonset.R'
  )
  
  source('call_grid.R')
  call_grid(cfg_grid)#default 4 chains
}

library(ggplot2)
library(rstan)
library(plyr)
source('scr_stan_fit_filename.R')


cfg = list(resample_to_fs =2,
           orgsamplingrate=10,
           adapt_delta=0.65,
           subject=1, 
           model='scr_model_1_hierarchicalonset.stan',
#model='scr_model_1_HO_noL_idx.stan',
           plot=T)

cfg$filename = scr_stan_fit_filename(cfg)
if(length(cfg$subject)>1) stop('please use the model 2 file for multiple subjects')
if(any(cfg$subject<0)){
  # negative subjects = simulation
  set.seed(1)
  source('scr_get_stan_data_simulation.R')
  data = scr_get_stan_data_simulation(cfg)
  
  qplot(x=1:500,y=data$data_stan$scr[1:500],geom='line')+geom_vline(xintercept=data$init.f$onsets)
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid())%%2^31) )
}else{
  
  # Real data
  source('scr_get_stan_data.R')
  data = scr_get_stan_data(cfg)
}


model<- stan_model(file = cfg$model)

parstosave = c('amp','latency','tau1','tau2','scr_sigma','amp_per_onset')
#parstosave = c('amp','tau1','tau2','scr_sigma','amp_per_onset')
fit2 <- sampling(model,
                data = data$data_stan, 
                algorithm='NUTS',
                iter = 1000,
                init = (rep(list(data$init.f),1)),
                chains = 1,
                cores=1,
                refresh=1,
                pars=parstosave)#,
                #control=list(adapt_delta=cfg$adapt_delta,max_treedepth=15,stepsize_jitter = 0.5,adapt_kappa=0.9))

#save(fit,cfg,file=cfg$filename)
