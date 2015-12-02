#!/usr/bin/Rscript
if (1==0){
  cfg = list(nchains = 1,
             gridOutputPath = file.path('./gridoutput'),
             requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
             job_name = 'scr_stan'
  )
  
  t = '1:4'
  #t = '1'
  cmd=paste0('qsub -cwd -t ',t, ' -o ', cfg$gridOutputPath, '/ -e ', cfg$gridOutputPath, '/ -l ', cfg$requirements, ' -N ', cfg$job_name, ' -pe default ', 1, ' -q nbp.q fit_data_model_2_hierarchicalonset.R')
  system(cmd)
}


library(ggplot2)
library(rstan)
library(plyr)
source('generate_scr_data.R')
source('scr_model.R')
source('scr_load_data.R')
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

cfg = list(resample_to_fs = 10,
           orgsamplingrate=10,
           adapt_delta=0.8,
           plot=F)

name = '2015-11-27_scr_model_2_hierarchicalonset'
name = paste0(name,'_adaptdelta',cfg$adapt_delta, 'grid_',paste0(sample(c(0:9, letters, LETTERS),10,replace=TRUE),collapse=''))
name = paste0(name,'.RData')
dir = './stanfit/'
dir.create(file.path(dir), showWarnings = F)

# LOAD DATA

out = scr_load_data(plot=cfg$plot,resample_to_fs = cfg$resample_to_fs)

data.scr = out$data.scr
data.scr.resamp = out$data.scr.resamp

# PREPARE STAN
source('scr_stan_data.R')
data_stan = scr_stan_data(data.scr,data.scr.resamp,cfg = cfg)
ggplot(data.frame(x=data_stan$x_per_subject[1:data_stan$ntime_per_subject[1]],scr=data_stan$scr[1:data_stan$ntime_per_subject[1]]))+geom_line(aes(x=x,y=scr))+
         geom_vline(data=data.frame(),aes(x=NULL,y=NULL,xintercept=data_stan$onset[1:data_stan$nonset[1]]))





source('scr_stan_init.R')
init.f = scr_stan_init(subject=NULL)
model_2 <- stan_model(file = 'scr_model_2_hierarchicalonset.stan')

# STAN FIT
parstosave = c('m_latency','s_latency','latency','m_amp','s_amp','m_amp_sigma','s_amp_sigma','amp','amp_sigma','amp_per_onset','m_tau1','s_tau1','tau1','m_tau2','s_tau2','tau2','m_scr_sigma','s_scr_sigma','scr_sigma')
fit <- sampling(model_2,data = data_stan, algorithm='NUTS',iter = 1000,init = list(init.f()),chains = 1,refresh=1,pars=parstosave,control=list(adapt_delta=cfg$adapt_delta))
                
if(1 == 0){
 # Plots just for diagnostics 
  traceplot(fit,pars=c('m_amp','m_latency','m_tau1','m_tau2','m_scr_sigma'),inc_warmup=T)
  traceplot(fit,pars=c('s_amp','s_latency','s_tau1','s_tau2','s_scr_sigma'),inc_warmup=T)
  
  sub = 2
  traceplot(fit,pars=laply(c('amp[%d,1]','latency[%d,1]','tau1[%d]','tau2[%d]','scr_sigma[%d]'),.fun=function(x)sprintf(x,sub)),inc_warmup=T)
  
  traceplot(fit,pars=laply('amp_per_onset[1,2]',.fun=function(x)sprintf(x,sub)),inc_warmup=T)
}

#SAVE FILES
save(fit,file= file.path(dir,name))
