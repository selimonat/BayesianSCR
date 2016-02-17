#!/usr/bin/Rscript
if (1==0){
  cfg_grid = list(nchains = 1,
                  gridOutputPath = file.path('./gridoutput'),
                  requirements = 'mem=3G,h=!ramsauer.ikw.uni-osnabrueck.de',
                  job_name = 'scr_group',
                  script_name = 'fit_data_model_2_hierarchicalonset.R'
  )
  source('call_grid.R')
  call_grid(cfg_grid,t='1:4')
}

# libraries
library(ggplot2)
library(rstan)
library(plyr)
source('scr_stan_fit_filename.R')

cfg = list(resample_to_fs =2,
           orgsamplingrate=10,
           adapt_delta=0.8,
           subject=1:10,
           model='scr_model_2_hierarchicalonset.stan',
           plot=T)

cfg$filename = scr_stan_fit_filename(cfg)


if(any(cfg$subject<0)){
  # negative subjects = simulation
  # set seed to always get the same data
  set.seed(1)
  source('scr_get_stan_data_simulation.R')
  data = scr_get_stan_data_simulation(cfg)
  qplot(x=1:500,y=data$data_stan$scr[1:500],geom='line')
  
  # set seed back to normal
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid())%%2^31) )
}else{
  # Real data
  source('scr_get_stan_data.R')
  data = scr_get_stan_data(cfg)
}



model<- stan_model(file = cfg$model)


parstosave = c('m_latency','s_latency','latency','m_amp','s_amp','m_amp_sigma','s_amp_sigma','amp','amp_sigma','amp_per_onset','m_tau1','s_tau1','tau1','m_tau2','s_tau2','tau2','m_scr_sigma','s_scr_sigma','scr_sigma')
parstosave = c('m_amp','s_amp','m_amp_sigma','s_amp_sigma','amp','amp_sigma','amp_per_onset','m_tau1','s_tau1','tau1','m_tau2','s_tau2','tau2','m_scr_sigma','s_scr_sigma','scr_sigma')

# fit <- sampling(model,
#                 data = data$data_stan, 
#                 algorithm='NUTS',
#                 iter = 1000,
#                 thin=1,
#                 init = list(data$init.f),
#                 chains = 1,
#                 refresh=10,
#                 pars=parstosave,
#                 control=list(adapt_delta=cfg$adapt_delta))
# 


#SAVE FILES
# save(fit,cfg,file=cfg$filename)
