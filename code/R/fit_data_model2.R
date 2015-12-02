library(ggplot2)
library(rstan)
library(plyr)
source('generate_scr_data.R')
source('scr_model.R')
source('scr_load_data')
set.seed(3)
# Generate Fake Data
out = scr_load_data(plot=T)
data.scr = out$data.scr
data.scr.resamp = out$data.scr.resamp
# Fit STAN Model


source('scr_stan_data.R')
data_stan = scr_stan_data(data.scr,data.scr.resamp,subselect = 19,cfg = cfg)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_2 <- stan_model(file = 'scr_model2.stan')
#model_2 <- stan_model(file = 'scr_model_2_beta_1.stan')
source('scr_stan_init.R')
init.f = scr_stan_init() # 
stop('not sure if this will work because the init has initvalues also for onset of amplitude, be careful')

fit.data <- sampling(model_2,data = data_stan,init= init.f, algorithm='NUTS',iter = 200, chains = 1,refresh=1,control=list(adapt_delta=0.9),verbose=T)


#summary(do.call(rbind, args = get_sampler_params(fit2, inc_warmup = TRUE)),
#        digits = 2)
source('posterior_predictive.R')
traceplot(fit2,pars=c('m_amp','m_latency','m_tau1','m_tau2','s_amp','s_latency','s_tau1','s_tau2'),inc_warmup=T)
a = posterior_predictive_group(fit2,1:cfg$ntime,onsets=data.onset,niter = 15)
#ggplot(data.frame(a),aes(x=time,y=scr))+geom_point()+facet_grid(subject~iter)
traceplot(fit2,pars=c('amp','latency','tau1','tau2'),inc_warmup=T)

# Evaluate Fit