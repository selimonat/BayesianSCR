#!/usr/bin/Rscript
if (1==0){
  cfg = list(nchains = 1,
             gridOutputPath = file.path('./gridoutput'),
             requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
             job_name = 'scr_stan'
  )
  
  t = '1:4'
  #t = '1'
  cmd=paste0('qsub -cwd -t ',t, ' -o ', cfg$gridOutputPath, '/ -e ', cfg$gridOutputPath, '/ -l ', cfg$requirements, ' -N ', cfg$job_name, ' -pe default ', 1, ' -q nbp.q .R')
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

source('scr_stan_data.R')
data_stan = scr_stan_data(data.scr,data.scr.resamp,subselect = 19,cfg = cfg)



adapt_delta=0.8
name = '2015-11-27_scr_model_2_hierarchicalonset'
name = paste0(name,'_adaptdelta',adapt_delta, 'grid_',paste0(sample(c(0:9, letters, LETTERS),10,replace=TRUE),collapse=''))
name = paste0(name,'.RData')
dir = './stanfit/'
dir.create(file.path(dir), showWarnings = F)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
init.f = function(chain_id){
  library(plyr)
  mlFit = read.csv('~/Documents/fancycarp/calibrationdata_56subjects_onsets.txt',header = F)
  colnames(mlFit) = c('idx','subject','latency','tau1','tau2','amp')
  mlFit$latency = mlFit$latency*10
  y =   ddply(mlFit,.(subject),summarise,l = mean(latency),t1=mean(tau1),t2=mean(tau2),a=mean(amp))
  l = list(
    m_tau1 = mean(y$t1),
    m_tau2 = mean(y$t2),
    m_latency = array(mean(y$l)),
    m_amp = array(mean(y$a)),
    m_scr_sigma = 0.7,
    s_tau1 = sd(y$t1),
    s_tau2 = sd(y$t2),
    s_latency = array(sd(y$l)),
    s_amp = array(sd(y$a)),
    s_scr_sigma = 0.5,
    tau1 = array(y$t1), #rep(mean(y$t1),length(y[,1])),#
    tau2 = array(y$t2),#rep(mean(y$t2),length(y[,1])),#
    latency = t(matrix(y$l,nrow=1)),
    
    amp = t(matrix(y$a,nrow=1)),
    scr_sigma = rep(0.7,length(y[,1])),
    amp_per_onset = matrix(mlFit$amp,nrow=1)
    
  )
  ix =y$t1-y$t2 < 0.1
  l$tau2[ix] = l$tau2[ix]*0.6
  #l$tau2[c(1:20,42:43,50,53)] = mean(y$t2)
  return(l)
}

model_2 <- stan_model(file = 'scr_model_2_hierarchicalonset_noLatency.stan')


parstosave = c('m_latency','s_latency','latency','m_amp','s_amp','m_amp_sigma','s_amp_sigma','amp','amp_sigma','amp_per_onset','m_tau1','s_tau1','tau1','m_tau2','s_tau2','tau2','m_scr_sigma','s_scr_sigma','scr_sigma')
fit <- sampling(model_2,data = data_stan, algorithm='NUTS',iter = 1000,init = list(init.f()),chains = 1,refresh=1,pars=parstosave,control=list(adapt_delta=0.2))

if(1 == 0){
  # Plots just for diagnostics 
  traceplot(fit,pars=c('m_amp','m_latency','m_tau1','m_tau2','m_scr_sigma'),inc_warmup=T)
  traceplot(fit,pars=c('s_amp','s_latency','s_tau1','s_tau2','s_scr_sigma'),inc_warmup=T)
  
  sub = 2
  traceplot(fit,pars=laply(c('amp[%d,1]','latency[%d,1]','tau1[%d]','tau2[%d]','scr_sigma[%d]'),.fun=function(x)sprintf(x,sub)),inc_warmup=T)
  
  traceplot(fit,pars=laply('amp_per_onset[1,2]',.fun=function(x)sprintf(x,sub)),inc_warmup=T)
}

#grid engine saving
save(fit,file= file.path(dir,name))
