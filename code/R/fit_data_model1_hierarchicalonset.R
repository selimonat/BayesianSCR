#!/usr/bin/Rscript
if (1==0){
  cfg = list(nchains = 1,
             gridOutputPath = file.path('./gridoutput'),
             requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
             job_name = 'scr_stan'
  )
  
  t = '1:5'
  #t = '1'
  cmd=paste0('qsub -cwd -t ',t, ' -o ', cfg$gridOutputPath, '/ -e ', cfg$gridOutputPath, '/ -l ', cfg$requirements, ' -N ', cfg$job_name, ' -pe default ', 1, ' -q nbp.q fit_data_model1_hierarchicalonset.R')
  system(cmd)
}


library(ggplot2)
library(rstan)
library(plyr)
source('generate_scr_data.R')
source('scr_model.R')
source('scr_load_data.R')


name = '2015-11-25_scr_model1_hierarchicalonset'
name = paste0(name, 'grid_',paste0(sample(c(0:9, letters, LETTERS),10,replace=TRUE),collapse=''))
name = paste0(name,'.RData')
dir = './stanfit/'
dir.create(file.path(dir), showWarnings = F)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# Generate Fake Data
#set.seed(1)
cfg = list(resample_to_fs = 1,
           orgsamplingrate = 10, 
           subject=15,
           plot=T)

out = scr_load_data(plot=cfg$plot,resample_to_fs = cfg$resample_to_fs)

data.scr = out$data.scr
data.scr.resamp = out$data.scr.resamp

source('scr_stan_data.R')
data_stan = scr_stan_data(data.scr,data.scr.resamp,subselect = cfg$subject,cfg = cfg)

#ggplot()+geom_line(aes(x=data_stan$x,y=data_stan$scr))+geom_vline(aes(xintercept=data_stan$onset))
source('scr_stan_init.R')
init.f=scr_stan_init(subject=cfg$subject)

model_1 <- stan_model(file = 'scr_model_1_hierarchicalonset.stan')
# Filepath for the grid engine

fit <- sampling(model_1,data = data_stan, algorithm='NUTS',iter = 3000, init = init.f,chains = 1,refresh=10,pars=c('amp','latency','tau1','tau2','scr_sigma','amp_per_onset'))

#grid engine saving
save(fit,file= file.path(dir,name))
