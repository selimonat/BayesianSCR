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



data_stan <- list(
  nsubject = length(unique(data.scr.resamp$subject)),
  nonset=(as.numeric(daply(data.scr,.(subject),summarise,sum(onset)))), # number of onsets * nconditions * subject, for the simulation only!
  ncondition = 1, # number of conditions
  ntime_per_subject =(as.numeric(daply(data.scr.resamp,.(subject),summarise,length(time)))),
  scr = data.scr.resamp$scr,
  x_per_subject =unlist(dlply(data.scr.resamp,.(subject),function(x){1:length(x$time)})),
  onset = unlist(dlply(data.scr,.(subject),function(x){which(x$onset==1)}))/50, #50 because we downsampled from 50 to 1
  condition = unlist(dlply(data.scr,.(subject),function(x){rep(1,sum(x$onset))}))
)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_2 <- stan_model(file = 'scr_model2.stan')
#model_2 <- stan_model(file = 'scr_model_2_beta_1.stan')
init.f = function(chain_id){
  cfg = list(nsub = 25)
  l = list(
    m_tau1 = 4,
    m_tau2 = 2,
    m_latency = array(c(10,15)),
    m_amp = array(c(6,3)),
    m_scr_sigma = 0.05,
    s_tau1 = 1,
    s_tau2 = 1,
    s_latency = array(c(1,1)),
    s_amp = array(c(.1,1)),
    s_scr_sigma = 0.1,
    tau1 = rep(4,cfg$nsub),
    tau2 = rep(2,cfg$nsub),
    latency = t(matrix(rep(c(10,15),cfg$nsub),nrow=2)),
    
    amp = t(matrix(rep(c(6,3),cfg$nsub),nrow=2)),
    
    scr_sigma = rep(0.05,cfg$nsub)
    
    
  )
  return(l)
}
fit.data <- sampling(model_2,data = data_stan, algorithm='NUTS',iter = 200, chains = 1,refresh=1,control=list(adapt_delta=0.9),verbose=T)


#summary(do.call(rbind, args = get_sampler_params(fit2, inc_warmup = TRUE)),
#        digits = 2)
source('posterior_predictive.R')
traceplot(fit2,pars=c('m_amp','m_latency','m_tau1','m_tau2','s_amp','s_latency','s_tau1','s_tau2'),inc_warmup=T)
a = posterior_predictive_group(fit2,1:cfg$ntime,onsets=data.onset,niter = 15)
#ggplot(data.frame(a),aes(x=time,y=scr))+geom_point()+facet_grid(subject~iter)
traceplot(fit2,pars=c('amp','latency','tau1','tau2'),inc_warmup=T)

# Evaluate Fit