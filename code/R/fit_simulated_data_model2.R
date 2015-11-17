
library(ggplot2)
library(rstan)
library(plyr)
source('generate_scr_data.R')
source('scr_model.R')
set.seed(3)
# Generate Fake Data
cfg = list(plotData = T,
           nsub = 20,
           nonsets = 2,
           ntime = 100)
data.onset = NULL
data.sim = NULL

for(sub in 1:cfg$nsub){
  # This data.frame defines when the onsets (randomly placed) and which conditions they are
  allonsets = c(0.01,0.20,0.40,0.60)*cfg$ntime#sample.int(0.1*cfg$ntime,2*cfg$nonsets)*8
  
  data = data.frame(condition = c(rep(1,cfg$nonsets),rep(2,cfg$nonsets)),
                    onset = c(onsetsA = allonsets[1:cfg$nonsets],
                              onsetsB = allonsets[(cfg$nonsets+1):(cfg$nonsets*2)]))
  
  data.sim = rbind(data.sim,data.frame(
    
    scr     = generate_scr_data(sigmaSCR=0.06,nT       = cfg$ntime, # Generate the data with the givven onsets, and number of samples
                                onsetsA = data$onset[data$condition==1],
                                onsetsB = data$onset[data$condition==2]),
    subject = sub,
    time    = 1:cfg$ntime))
  
  
  data.onset = rbind(data.onset,cbind(data,subject=sub))
}
if(cfg$plotData) ggplot(data.sim,aes(x=time,y=scr,color=factor(subject)))+geom_point()+facet_grid(subject~.)

# Fit STAN Model



data_stan <- list(
  nsubject = cfg$nsub,
  ntrial=as.numeric(daply(data.onset,.(subject),summarise,length(condition))), # number of onsets * nconditions * subject, for the simulation only!
  ncondition = 2, # number of conditions
  ntime_per_subject =as.numeric(daply(data.sim,.(subject),summarise,length(time))),
  scr = data.sim$scr,
  x_per_subject = data.sim$time,
  onset = data.onset$onset, # 
  condition = data.onset$condition # which Condition [a/b]?
)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_2 <- stan_model(file = 'scr_model2.stan')

init.f = function(chain_id){
  l = list(
    m_tau1 = 10,
    m_tau2 = 3,
    m_latency = c(10,10),
    m_amp = c(1,1),
    m_scr_sigma = 0.06,
    s_tau1 = .1,
    s_tau2 = .1,
    s_latency = c(1,1),
    s_amp = c(.1,.1),
    s_scr_sigma = 0.1
  )
  return(l)
}
fit2 <- sampling(model_2,data = data_stan, algorithm='NUTS',iter = 10, chains = 1,refresh=1,control=list(adapt_delta=0.8),verbose=T)
#fit2 <- sampling(model_2,data = data_stan, algorithm='NUTS',init=init.f,iter = 500, chains = 1,refresh=10,control=list(adapt_delta=0.8))

#summary(do.call(rbind, args = get_sampler_params(fit2, inc_warmup = TRUE)),
#        digits = 2)

traceplot(fit2,pars=c('m_amp','m_latency','m_tau1','m_tau2','s_amp','s_latency','s_tau1','s_tau2'),inc_warmup=T)
a = posterior_predictive_group(fit2,1:cfg$ntime,onsets=data.onset,niter = 15)
ggplot(data.frame(a),aes(x=time,y=scr))+geom_point()+facet_grid(subject~iter)
#traceplot(fit2,pars=c('amp','latency','tau1','tau2'),inc_warmup=T)

# Evaluate Fit