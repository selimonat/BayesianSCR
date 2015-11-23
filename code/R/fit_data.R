#!/usr/bin/Rscript
if (1==0){
  cfg = list(nchains = 1,
             gridOutputPath = file.path('./gridoutput'),
             requirements = 'mem=2G,h=!ramsauer.ikw.uni-osnabrueck.de',
             job_name = 'scr_stan'
  )
  
  t = '1:4'
  #t = '1'
  cmd=paste0('qsub -cwd -t ',t, ' -o ', cfg$gridOutputPath, '/ -e ', cfg$gridOutputPath, '/ -l ', cfg$requirements, ' -N ', cfg$job_name, ' -pe default ', 1, ' -q nbp.q fit_data.R')
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
out = scr_load_data(plot=F)
data.scr = out$data.scr
data.scr.resamp = out$data.scr.resamp


data.scrSL = subset(data.scr,data.scr$subject==19)
data.scrSL.resamp = subset(data.scr.resamp,data.scr.resamp$subject==19)

data_stan <- list(
  ntime=length(data.scrSL.resamp$time), # number of timepoints
  ntrial=sum(data.scrSL$onset), # number of onsets * nconditions
  ncondition = 1, # number of conditions
  scr = data.scrSL.resamp$scr,
  x = 1:length(data.scrSL.resamp$scr),
  onset = which(data.scrSL$onset==1)/50, # 
  condition = rep(1,sum(data.scrSL$onset)) # which Condition [a/b]?
)

ggplot()+geom_line(aes(x=data_stan$x,y=data_stan$scr))+geom_vline(aes(xintercept=data_stan$onset))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
init.f = function(chain_id){
  l = list(
    tau1 = 4,
    tau2 = 3,
    latency = array(c(10)),
    amp = array(c(1)),
    scr_sigma = 0.06
  )
  return(l)
}

model_1 <- stan_model(file = 'scr_model1.stan')
# Filepath for the grid engine
name = '2015-11-23_scr_model1'
name = paste0(name, 'grid_',paste0(sample(c(0:9, letters, LETTERS),10,replace=TRUE),collapse=''))
name = paste0(name,'.RData')
dir = './stanfit/'
dir.create(file.path(dir), showWarnings = F)
  

fit <- sampling(model_1,data = data_stan, algorithm='NUTS',iter = 1000, init = init.f,chains = 1,refresh=1,pars=c('amp','latency','tau1','tau2','scr_sigma'),control=list(adapt_delta=0.5))

#grid engine saving
save(fit,file= file.path(dir,name))

# Plotting
traceplot(fit,pars=c('amp','latency','tau1','tau2','scr_sigma'),inc_warmup=T)

# Evaluate Fit
source('posterior_predictive.R')
a = data.frame(posterior_predictive_single(fit,1:data_stan$ntime,onsets=data.frame(onset=data_stan$onset,condition=1)))
b = rbind.fill(cbind(a,type='post'),cbind(data.scrSL.resamp,type='raw'))
b = cbind(b,alpha <- ifelse(b$type=='post', 0.1, 1))

ggplot(b,aes(x=time,y=scr,colour=type))+stat_summary(fun.ymin="min",fun.ymax="max",fun.y=median,geom='errorbar')+geom_line()
ggplot(b,aes(x=time,y=scr,colour=type,group=iter))+geom_line(aes(alpha=alpha))

#ggplot(data.frame(a),aes(x=time,y=scr,group=iter))+geom_point()+facet_wrap(~iter,ncol = 10)
