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
# Generate Fake Data
#set.seed(1)
cfg = list(resample_to_fs = 10,
           plot=F)
out = scr_load_data(plot=cfg$plot,resample_to_fs = cfg$resample_to_fs)

data.scr = out$data.scr
data.scr.resamp = out$data.scr.resamp


data.scrSL = subset(data.scr,data.scr$subject==19)
data.scrSL.resamp = subset(data.scr.resamp,data.scr.resamp$subject==19)

data_stan <- list(
  ntime=length(data.scrSL.resamp$time), # number of timepoints
  ntrial=sum(data.scrSL$onset), # number of onsets * nconditions
  ncondition = 1, # number of conditions
  scr = data.scrSL.resamp$scr,
  x = 0:(length(data.scrSL.resamp$scr)-1),
  onset = which(data.scrSL$onset==1)/(50/cfg$resample_to_fs), # 
  condition = rep(1,sum(data.scrSL$onset)) # which Condition [a/b]?
)

#ggplot()+geom_line(aes(x=data_stan$x,y=data_stan$scr))+geom_vline(aes(xintercept=data_stan$onset))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
init.f = function(chain_id){
  l = list(
    tau1 = 5,
    tau2 = 2,
    latency = array(c(3)),
    amp = array(c(1.5)),
    amp_sigma = array(c(1)),
    amp_per_onset = matrix(rep(1.5,data_stan$ntrial),nrow=1),
    scr_sigma = 0.6
  )
  return(l)
}

model_1 <- stan_model(file = 'scr_model_1_hierarchicalonset.stan')
# Filepath for the grid engine
name = '2015-11-25_scr_model1_hierarchicalonset'
name = paste0(name, 'grid_',paste0(sample(c(0:9, letters, LETTERS),10,replace=TRUE),collapse=''))
name = paste0(name,'.RData')
dir = './stanfit/'
dir.create(file.path(dir), showWarnings = F)


fit <- sampling(model_1,data = data_stan, algorithm='NUTS',iter = 3000, init = init.f,chains = 1,refresh=10,pars=c('amp','latency','tau1','tau2','scr_sigma','amp_per_onset'))

#grid engine saving
save(fit,file= file.path(dir,name))
