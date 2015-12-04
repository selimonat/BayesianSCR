#!/usr/bin/Rscript
if (1==0){
  cfg_grid = list(nchains = 1,
             gridOutputPath = file.path('./gridoutput'),
             requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
             job_name = 'scr_singlesub',
             script_name = 'fit_data_model_1_hierarchicalonset.R'
  )
  
  source('call_grid')
  call_grid(cfg_grid)
}

library(ggplot2)
library(rstan)
library(plyr)
source('scr_stan_fit_filename.R')


cfg = list(resample_to_fs =2,
           orgsamplingrate=10,
           adapt_delta=0.8,
           subject=42,
           model='scr_model_1_hierarchicalonset.stan',
           plot=T)

cfg$filename = scr_stan_fit_filename(cfg)
source('scr_get_stan_data.R')
data = scr_get_stan_data(cfg)


model<- stan_model(file = cfg$model)

parstosave = c('amp','latency','tau1','tau2','scr_sigma','amp_per_onset')
fit <- sampling(model,
                data = data$data_stan, 
                algorithm='NUTS',
                iter = 1000,
                init = list(data$init.f),
                chains = 1,
                refresh=10,
                pars=parstosave,
                control=list(adapt_delta=cfg$adapt_delta))


save(fit,file=cfg$filename)