#!/usr/bin/Rscript
if (1==0){
  cfg_grid = list(nchains = 1,
             gridOutputPath = file.path('./gridoutput'),
             requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
             job_name = 'scr_group'
  )
  
  t = '1:4'
  #t = '1'
  cmd=paste0('qsub -cwd -t ',t,
             ' -o ', cfg_grid$gridOutputPath, '/ -e ', cfg_grid$gridOutputPath, '/',
             ' -l ', cfg_grid$requirements, 
             ' -N ', cfg_grid$job_name, 
             ' -pe default ', 1,
             ' -q nbp.q fit_data_model_2_hierarchicalonset_noLat.R')
  system(cmd)
}

# libraries
library(ggplot2)
library(rstan)
library(plyr)
source('scr_stan_fit_filename.R')

cfg = list(resample_to_fs =2,
           orgsamplingrate=10,
           adapt_delta=0.8,
           model='scr_model_2_hierarchicalonset_noLatency.stan',
           plot=T)

cfg$filename = scr_stan_fit_filename(cfg)
source('scr_get_stan_data.R')
data = scr_get_stan_data(cfg)


model<- stan_model(file = cfg$model)

parstosave = c('m_amp','s_amp','m_amp_sigma','s_amp_sigma','amp','amp_sigma','amp_per_onset','m_tau1','s_tau1','tau1','m_tau2','s_tau2','tau2','m_scr_sigma','s_scr_sigma','scr_sigma')
#parstosave = c('m_latency','s_latency','latency','m_amp','s_amp','m_amp_sigma','s_amp_sigma','amp','amp_sigma','amp_per_onset','m_tau1','s_tau1','tau1','m_tau2','s_tau2','tau2','m_scr_sigma','s_scr_sigma','scr_sigma')

fit <- sampling(model,
                data = data$data_stan, 
                algorithm='NUTS',
                iter = 1000,
                init = list(data$init.f),
                chains = 1,
                refresh=10,
                pars=parstosave,
                control=list(adapt_delta=cfg$adapt_delta))



#SAVE FILES
save(fit,file=cfg$filename)
